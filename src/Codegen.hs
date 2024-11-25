module Codegen where

import Ast
import Control.Monad.State
import qualified Data.ByteString.Short as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
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

newtype Env = Env {operands :: Map Identifier AST.Operand}

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

convType :: MonadState Env m => Type -> m AST.Type
convType t = case t of
  Void -> pure AST.void
  Int -> pure AST.i32
  Char -> pure AST.i8
  Bool -> pure AST.i1
  Float -> pure AST.double
  _ -> error $ "invalid type: " ++ show t

registerOperand :: MonadState Env m => Identifier -> AST.Operand -> m ()
registerOperand ident op = modify $ \env -> env {operands = M.insert ident op (operands env)}

codegenProgram :: SProgram -> AST.Module
codegenProgram (SProgram decls) =
  flip evalState (Env {operands = M.empty}) $
    L.buildModuleT "cluck" $ mapM_ codegenDecl decls

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
codegenExpr (Int, SIntLiteral n) = pure $ L.int32 (fromIntegral n)
codegenExpr (Float, SFloatLiteral n) = pure $ L.double n
codegenExpr (_, SVariableExpr ident) = do
  op <- gets (fromJust . M.lookup ident . operands)
  L.load op 0
codegenExpr (t, SFunctionExpr ident args) = do
  op <- gets (fromJust . M.lookup ident . operands)
  args' <- mapM codegenExpr args
  L.call op $ map (,[]) args'
codegenExpr (Bool, SBoolLiteral b) = pure $ L.bit $ if b then 1 else 0
codegenExpr (Char, SCharLiteral c) = pure $ L.int8 $ fromIntegral $ fromEnum c
codegenExpr (_, SBinaryOp op lex rex) = do
  lhs <- codegenExpr lex
  rhs <- codegenExpr rex
  case op of
    Add -> case (fst lex, fst rex) of
      (Int, Int) -> L.add lhs rhs
      (Float, Float) -> L.fadd lhs rhs
      _ -> error "internal error: semant failed"
    Sub -> case (fst lex, fst rex) of
      (Int, Int) -> L.sub lhs rhs
      (Float, Float) -> L.fsub lhs rhs
      _ -> error "internal error: semant failed"
    Mul -> case fst lex of
      Int -> L.mul lhs rhs
      Float -> L.fmul lhs rhs
      _ -> error "internal error: semant failed"
    Div -> case fst lex of
      Int -> L.sdiv lhs rhs
      Float -> L.fdiv lhs rhs
      _ -> error "internal error: semant failed"
    EqTo -> case fst lex of
      Int -> L.icmp IP.EQ lhs rhs
      Char -> L.icmp IP.EQ lhs rhs
      Float -> L.fcmp FP.OEQ lhs rhs
      _ -> error "internal error: semant failed"
    NtEqTo -> case fst lex of
      Int -> L.icmp IP.NE lhs rhs
      Char -> L.icmp IP.NE lhs rhs
      Float -> L.fcmp FP.ONE lhs rhs
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
    Not -> case t of
      Bool -> L.xor ex' (L.bit 1)
      _ -> error "internal error: semant failed"
    Ref -> codegenExpr ex
    _ -> error "internal error: semant failed"
codegenExpr _ = error "internal error: semant failed"

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

  bodyBlock <- L.block `L.named` "body"
  codegenStmt body
  c <- codegenExpr cond
  mkTerminator $ L.condBr c bodyBlock endBlock

  endBlock <- L.block `L.named` "end"
  pure ()
codegenStmt (SVariableDeclStmt (SVariableDecl t ident expr)) = do
  ty <- convType t
  addr <- L.alloca ty Nothing 0
  modify $ \env -> env {operands = M.insert ident addr (operands env)} -- add memory address to env
  case expr of
    Nothing -> pure ()
    Just e -> do
      op <- codegenExpr e
      L.store addr 0 op
codegenStmt (SVariableAssignStmt ident expr) = do
  operands' <- gets operands
  case M.lookup ident operands' of
    Just addr -> do
      op' <- codegenExpr expr
      L.store addr 0 op'
    Nothing -> error "error: semant failed"
codegenStmt _ = undefined

mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr
