module Codegen where

import Ast
import Control.Monad.State
import qualified Data.ByteString.Short as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
import Data.Text (unpack)
import qualified Data.Text.Encoding as TE
import qualified LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as AST
import qualified LLVM.IRBuilder.Constant as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import Sast

data Env = Env
  { operands :: Map Identifier AST.Operand,
    breakLabel :: AST.Name,
    continueLabel :: AST.Name
  }

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

convType :: MonadState Env m => Type -> m AST.Type
convType (Pointer Void) = pure $ AST.ptr AST.i8
convType t = case t of
  Void -> pure AST.void
  Int -> pure AST.i32
  Char -> pure AST.i8
  Bool -> pure AST.i1
  Float -> pure AST.double
  Pointer ty -> AST.ptr <$> convType ty
  Array ty (Just size) -> AST.ArrayType (fromIntegral size) <$> convType ty
  Array ty Nothing -> AST.ArrayType 0 <$> convType ty

emitBuiltIn :: (Identifier, [AST.Type], AST.Type) -> LLVM ()
emitBuiltIn (name, args, rett) = do
  func <- L.extern ((AST.mkName . unpack) name) args rett
  modify $ \env -> env {operands = M.insert name func (operands env)}

codegenProgram :: SProgram -> AST.Module
codegenProgram (SProgram decls) =
  flip evalState (Env {operands = M.empty, breakLabel = "", continueLabel = ""}) $
    L.buildModuleT "cluck" $ do
      mapM_
        emitBuiltIn
        [ ("printint", [AST.i32], AST.void),
          ("printfloat", [AST.double], AST.void),
          ("malloc", [AST.i32], AST.ptr AST.i8),
          ("free", [AST.ptr AST.i8], AST.void),
          ("scanf", [AST.ptr AST.i8, AST.ptr AST.i8], AST.i32),
          ("sqrt", [AST.double], AST.double)
        ]
      printf <- L.externVarArgs (AST.mkName "printf") [AST.ptr AST.i8] AST.i32
      modify $ \env -> env {operands = M.insert "printf" printf (operands env)}
      mapM_ codegenDecl decls

codegenLVal :: LVal -> Codegen AST.Operand
codegenLVal (LVar ident) = do
  ops <- gets operands
  case M.lookup ident ops of
    Just op -> pure op
    Nothing -> error "error: semant failed"
codegenLVal (LDeref expr) = codegenExpr expr
codegenLVal (LArray ident i) = do
  index <- codegenExpr i
  ops <- gets operands
  case M.lookup ident ops of
    Just op -> do
      addr <- L.load op 0
      L.gep addr [index]
    Nothing -> error "error: semant failed"

codegenDecl :: SDecl -> LLVM ()
codegenDecl (SFunctionDecl t ident args b) = mdo
  modify $ \env -> env {operands = M.insert ident function (operands env)} -- add memory address to env
  oldState <- get

  function <- do
    rett <- convType t
    args' <- mapM mkArg args
    L.function (AST.mkName $ cs ident) args' rett genBody
  put oldState
  where
    mkArg (t', ident') = (,) <$> convType t' <*> pure (L.ParameterName (BS.toShort (TE.encodeUtf8 ident')))
    genBody :: [AST.Operand] -> Codegen ()
    genBody ops = do
      _ <- L.block `L.named` "entry"
      mapM_ initParam $ zip args ops
      codegenStmt b
      where
        initParam ((t', i), op) = do
          ty <- convType t'
          addr <- L.alloca ty Nothing 0
          L.store addr 0 op
          modify $ \env -> env {operands = M.insert i addr (operands env)}
codegenDecl (SVariableDecl t ident e) = do
  let name = AST.mkName $ cs ident
      val = case e of
        Just (_, expr) -> case expr of
          (SIntLiteral n) -> C.Int 32 $ fromIntegral n
          (SFloatLiteral d) -> C.Float $ F.Double d
          (SBoolLiteral b) -> C.Int 1 $ if b then 1 else 0
          (SCharLiteral c) -> C.Int 8 $ (fromIntegral . fromEnum) c
          _ -> error "error: semant failed"
        Nothing -> case t of
          Int -> C.Int 0 0
          Float -> C.Float $ F.Double 0
          Bool -> C.Int 0 0
          Char -> C.Int 0 0
          _ -> error "error: semant failed"

  ty <- convType t
  var <- L.global name ty val
  modify $ \env -> env {operands = M.insert ident var (operands env)}

codegenExpr :: SExpr -> Codegen AST.Operand
codegenExpr (_, SIntLiteral n) = pure $ L.int32 (fromIntegral n)
codegenExpr (_, SFloatLiteral n) = pure $ L.double n
codegenExpr (_, SBoolLiteral b) = pure $ L.bit $ if b then 1 else 0
codegenExpr (_, SCharLiteral c) = pure $ L.int8 $ fromIntegral $ fromEnum c
codegenExpr (_, SStringLiteral s) = do
  let elems = unpack s ++ "\0"
  let c = C.Array AST.i8 $ map (C.Int 8 . fromIntegral . fromEnum) (unpack s ++ "\0")
  ty <- convType (Array Char (Just (length elems)))
  addr <- L.alloca ty Nothing 0
  L.store addr 0 (AST.ConstantOperand c)
  L.gep addr [L.int64 0, L.int64 0]
codegenExpr (t, SNull) = do
  ty <- convType t
  L.inttoptr (L.int64 0) ty
codegenExpr (_, LVal l) = do
  op <- codegenLVal l
  L.load op 0
codegenExpr (_, SFunctionExpr ident args) = do
  op <- gets (fromJust . M.lookup ident . operands)
  args' <- mapM codegenExpr args
  L.call op $ map (,[]) args'
codegenExpr (_, SAssign lex rex) = do
  lhs <- codegenLVal lex
  rhs <- codegenExpr rex
  L.store lhs 0 rhs
  return rhs
codegenExpr (_, SRef l) = codegenLVal l
codegenExpr (_, SBinaryOp op lex rex) = do
  lhs <- codegenExpr lex
  rhs <- codegenExpr rex
  case op of
    Add -> case (fst lex, fst rex) of
      (Int, Int) -> L.add lhs rhs
      (Float, Float) -> L.fadd lhs rhs
      (Pointer _, Int) -> L.gep lhs [rhs]
      (Int, Pointer _) -> L.gep lhs [rhs]
      _ -> error "internal error: semant failed"
    Sub -> case (fst lex, fst rex) of
      (Int, Int) -> L.sub lhs rhs
      (Float, Float) -> L.fsub lhs rhs
      (Pointer _, Int) -> do
        rhs' <- L.sub (L.int32 0) rhs
        L.gep lhs [rhs']
      _ -> error "internal error: semant failed"
    Mul -> case fst lex of
      Int -> L.mul lhs rhs
      Float -> L.fmul lhs rhs
      _ -> error "internal error: semant failed"
    Div -> case fst lex of
      Int -> L.sdiv lhs rhs
      Float -> L.fdiv lhs rhs
      _ -> error "internal error: semant failed"
    Mod -> case fst lex of
      Int -> L.srem lhs rhs
      Float -> L.frem lhs rhs
      _ -> error "internal error: semant failed"
    EqTo -> case fst lex of
      Int -> L.icmp IP.EQ lhs rhs
      Char -> L.icmp IP.EQ lhs rhs
      Float -> L.fcmp FP.OEQ lhs rhs
      Bool -> L.icmp IP.EQ lhs rhs
      Pointer _ -> L.icmp IP.EQ lhs rhs
      _ -> error "internal error: semant failed"
    NtEqTo -> case fst lex of
      Int -> L.icmp IP.NE lhs rhs
      Char -> L.icmp IP.NE lhs rhs
      Float -> L.fcmp FP.ONE lhs rhs
      Bool -> L.icmp IP.NE lhs rhs
      Pointer _ -> L.icmp IP.NE lhs rhs
      _ -> error "internal error: semant failed"
    Lt -> case fst lex of
      Int -> L.icmp IP.SLT lhs rhs
      Char -> L.icmp IP.ULT lhs rhs
      Float -> L.fcmp FP.OLT lhs rhs
      _ -> error "internal error: semant failed"
    LtOrEqTo -> case fst lex of
      Int -> L.icmp IP.SLE lhs rhs
      Char -> L.icmp IP.ULE lhs rhs
      Float -> L.fcmp FP.OLT lhs rhs
      _ -> error "internal error: semant failed"
    Gt -> case fst lex of
      Int -> L.icmp IP.SGT lhs rhs
      Char -> L.icmp IP.UGT lhs rhs
      Float -> L.fcmp FP.OGT lhs rhs
      _ -> error "internal error: semant failed"
    GtOrEqTo -> case fst lex of
      Int -> L.icmp IP.SGE lhs rhs
      Char -> L.icmp IP.UGE lhs rhs
      Float -> L.fcmp FP.OGE lhs rhs
      _ -> error "internal error: semant failed"
    And -> L.and lhs rhs
    Or -> L.or lhs rhs
    _ -> error "internal error: semant failed"
codegenExpr (t, SUnaryOp op ex) = do
  ex' <- codegenExpr ex
  case op of
    Neg -> case t of
      Int -> L.sub (L.int32 0) ex'
      Float -> L.sub (L.double 0) ex'
      _ -> error "internal error: semant failed"
    Not -> L.xor ex' (L.bit 1)
    _ -> error "internal error: semant failed"
codegenExpr (t, SInc lval) = do
  addr <- codegenLVal lval
  op <- L.load addr 0
  after <- case t of
    Float -> L.add op (L.double 1)
    _ -> L.add op (L.int32 1)
  L.store addr 0 after
  return op
codegenExpr (t, SDec lval) = do
  addr <- codegenLVal lval
  op <- L.load addr 0
  after <- case t of
    Float -> L.sub op (L.double 1)
    _ -> L.sub op (L.int32 1)
  L.store addr 0 after
  return op
codegenExpr (_, SCast t1 expr@(t2, _)) = do
  op <- codegenExpr expr
  ty <- convType t1
  case (t1, t2) of
    (Pointer _, Pointer _) -> L.bitcast op ty
    (Pointer _, Int) -> L.inttoptr op ty
    (Int, Pointer _) -> L.ptrtoint op ty
    (Char, Int) -> L.trunc op ty
    (Int, Char) -> L.zext op ty
    (Float, Int) -> L.sitofp op ty
    (Int, Float) -> L.fptosi op ty
    _ -> error "error: semant failed"
codegenExpr (_, SSizeOf t) =
  case t of
    Char -> pure $ L.int32 1
    Int -> pure $ L.int32 4
    Float -> pure $ L.int32 8
    Bool -> pure $ L.int32 1
    Void -> pure $ L.int32 0
    Pointer _ -> pure $ L.int32 8
    _ -> error "error: semant failed"

codegenStmt :: SStmt -> Codegen ()
codegenStmt (SExprStmt expr) = void $ codegenExpr expr
codegenStmt (SReturnStmt expr) = case expr of
  Nothing -> L.retVoid
  Just e -> do
    op <- codegenExpr e
    L.ret op
codegenStmt (SBlockStmt stmts) = do
  let (l, r) = break isReturn stmts -- filter out unreachable statements after return
  mapM_ codegenStmt (l ++ take 1 r)
  where
    isReturn :: SStmt -> Bool
    isReturn (SReturnStmt _) = True
    isReturn _ = False
codegenStmt (SIfStmt cond t e) = mdo
  c <- codegenExpr cond
  L.condBr c thenBlock elseBlock

  thenBlock <- L.block `L.named` "then"
  codegenStmt t
  mkTerminator $ L.br mergeBlock

  elseBlock <- L.block `L.named` "else"
  codegenStmt e
  mkTerminator $ L.br mergeBlock

  mergeBlock <- L.block `L.named` "merge"
  pure ()
codegenStmt (SDoWhileStmt body cond) = mdo
  L.br bodyBlock
  oldEnv <- get
  modify $ \env -> env {breakLabel = endBlock, continueLabel = bodyBlock}

  bodyBlock <- L.block `L.named` "body"
  codegenStmt body
  c <- codegenExpr cond
  mkTerminator $ L.condBr c bodyBlock endBlock

  endBlock <- L.block `L.named` "end"
  modify $ const oldEnv
  pure ()
codegenStmt (SVariableDeclStmt t ident expr) = do
  ty <- convType t
  addr <- L.alloca ty Nothing 0
  modify $ \env -> env {operands = M.insert ident addr (operands env)} -- add memory address to env
  case expr of
    Nothing -> pure ()
    Just e -> do
      op <- codegenExpr e
      L.store addr 0 op
codegenStmt (SArrayDeclStmt t ident size items) = do
  ty <- convType (Array t (Just size))
  addr <- L.alloca ty Nothing 0
  decayTy <- convType (Pointer t)

  decayPtr <- L.alloca decayTy Nothing 0 -- init decay pointer
  decayVal <- L.gep addr [L.int64 0, L.int64 0]
  L.store decayPtr 0 decayVal

  modify $ \env -> env {operands = M.insert ident decayPtr (operands env)}
  case items of
    Nothing -> pure ()
    Just arr -> do
      ops <- mapM codegenExpr arr
      mapM_ -- initialise array by storing each operand at its respective index
        ( \(op, i) -> do
            ep <- L.gep addr [L.int64 0, L.int64 i]
            L.store ep 0 op
            pure ()
        )
        $ zip ops [0 ..]
codegenStmt SBreakStmt = do
  exit <- gets breakLabel
  L.br exit
codegenStmt SContinueStmt = do
  cont <- gets continueLabel
  L.br cont

mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr
