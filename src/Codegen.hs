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

-- keeps track of program state while traversing it
data Env = Env
  { operands :: Map Identifier AST.Operand,
    breakLabel :: AST.Name,
    continueLabel :: AST.Name
  }

-- type aliases to reduce type repetition
type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

-- converts a cluck AST type to an LLVM type
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

genConstant :: SExpr -> C.Constant
genConstant e = case e of
  (_, SIntLiteral n) -> C.Int 32 $ fromIntegral n
  (_, SFloatLiteral d) -> C.Float $ F.Double d
  (_, SBoolLiteral b) -> C.Int 1 $ if b then 1 else 0
  (_, SCharLiteral c) -> C.Int 8 $ (fromIntegral . fromEnum) c
  _ -> error "error: semant failed"

-- calculates size of a type in bytes (sizeof operator)
sizeof :: Type -> Int
sizeof t = case t of
  Char -> 1
  Int -> 4
  Float -> 8
  Bool -> 1
  Void -> 0
  Pointer _ -> 8
  Array t' (Just size) -> size * sizeof t'
  _ -> error "error: semant failed"

-- declares an external function in the LLVM IR
emitBuiltIn :: (Identifier, [AST.Type], AST.Type) -> LLVM ()
emitBuiltIn (name, args, rett) = do
  func <- L.extern ((AST.mkName . unpack) name) args rett
  modify $ \env -> env {operands = M.insert name func (operands env)}

-- builds an LLVM module from the AST definitions
codegenProgram :: SProgram -> AST.Module
codegenProgram (SProgram decls) =
  flip evalState (Env {operands = M.empty, breakLabel = "", continueLabel = ""}) $
    L.buildModuleT "cluck" $ do
      mapM_
        emitBuiltIn
        [ ("malloc", [AST.i32], AST.ptr AST.i8), -- list of built-in functions aside from printf and scanf
          ("free", [AST.ptr AST.i8], AST.void),
          ("sqrt", [AST.double], AST.double),
          ("pow", [AST.double, AST.double], AST.double)
        ]
      -- prinf and scanf are emitted separately as they use varargs
      printf <- L.externVarArgs (AST.mkName "printf") [AST.ptr AST.i8] AST.i32
      scanf <- L.externVarArgs (AST.mkName "scanf") [AST.ptr AST.i8] AST.i32
      modify $ \env -> env {operands = M.insert "scanf" scanf $ M.insert "printf" printf (operands env)}
      mapM_ codegenDecl decls

-- returns the memory address of an LVal, anything on the left-hand side of an assignment expression
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
      L.gep addr [index] -- getelementptr fetches the value at a memory index
    Nothing -> error "error: semant failed"

codegenDecl :: SDecl -> LLVM ()
codegenDecl (SFunctionDecl t ident args b) = mdo
  {- mdo is used for recursive definitions, the function needs to be added to the Env before codegen so that
     the function can call itself recursively -}
  modify $ \env -> env {operands = M.insert ident function (operands env)} -- add memory address to env
  oldState <- get

  function <- do
    rett <- convType t
    args' <- mapM mkArg args
    L.function (AST.mkName $ cs ident) args' rett genBody -- declare function in LLVM IR
  put oldState
  where
    mkArg (t', ident') = (,) <$> convType t' <*> pure (L.ParameterName (BS.toShort (TE.encodeUtf8 ident')))
    genBody ops = do
      _ <- L.block `L.named` "entry" -- define an entry block where function execution starts
      mapM_ initParam $ zip args ops
      codegenStmt b -- generate code for the function body
      where
        -- function parameters need to be initialised in the body so that they can be accessed
        initParam ((t', i), op) = do
          ty <- convType t'
          addr <- L.alloca ty Nothing 0 -- allocate memory for the parameter
          L.store addr 0 op
          modify $ \env -> env {operands = M.insert i addr (operands env)}
codegenDecl (SVariableDecl t ident e) = do
  let name = AST.mkName $ cs ident
      val = case e of
        Just expr -> genConstant expr -- generate constant as global variables can only have constant values
        Nothing -> case t of -- initalise variable with empty value if it has no value
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
  let elems = unpack s ++ "\0" -- add null terminator to strings by default
  let c = C.Array AST.i8 $ map (C.Int 8 . fromIntegral . fromEnum) elems
  ty <- convType (Array Char (Just (length elems)))
  addr <- L.alloca ty Nothing 0 -- allocate array and store string contents
  L.store addr 0 (AST.ConstantOperand c)
  L.gep addr [L.int64 0, L.int64 0] -- return pointer to first character
codegenExpr (t, SNull) = do
  ty <- convType t
  L.inttoptr (L.int64 0) ty -- null pointer value
codegenExpr (_, LVal l) = do
  op <- codegenLVal l
  L.load op 0 -- load value at LVal memory address
codegenExpr (_, SFunctionExpr ident args) = do
  op <- gets (fromJust . M.lookup ident . operands)
  args' <- mapM codegenExpr args
  L.call op $ map (,[]) args'
codegenExpr (_, SAssign lex rex) = do
  -- store rhs at the address of lhs
  lhs <- codegenLVal lex
  rhs <- codegenExpr rex
  L.store lhs 0 rhs
  return rhs
codegenExpr (_, SRef l) = codegenLVal l -- return memory address of value
codegenExpr (_, SBinaryOp op lex rex) = do
  lhs <- codegenExpr lex
  rhs <- codegenExpr rex
  -- pair up types with corresponding LLVM instruction
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
      Float -> L.fsub (L.double 0) ex'
      _ -> error "internal error: semant failed"
    Not -> L.xor ex' (L.bit 1)
    _ -> error "internal error: semant failed"
codegenExpr (t, SInc lval) = do
  -- load value at LVal
  addr <- codegenLVal lval
  op <- L.load addr 0
  -- increase value by 1 and store it at memory location
  after <- case t of
    Float -> L.add op (L.double 1)
    _ -> L.add op (L.int32 1)
  L.store addr 0 after
  return op -- return original value
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
codegenExpr (_, SSizeOf t) = pure $ (L.int32 . fromIntegral . sizeof) t

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
    isReturn (SReturnStmt _) = True
    isReturn _ = False
codegenStmt (SIfStmt cond t e) = mdo
  c <- codegenExpr cond
  L.condBr c thenBlock elseBlock -- goes to thenBlock if condition is true else elseBlock

  thenBlock <- L.block `L.named` "then"
  codegenStmt t
  mkTerminator $ L.br mergeBlock

  elseBlock <- L.block `L.named` "else"
  codegenStmt e
  mkTerminator $ L.br mergeBlock

  mergeBlock <- L.block `L.named` "merge" -- both paths merge after condition
  pure ()
codegenStmt (SSwitchStmt e cs') = mdo
  cond <- codegenExpr e
  L.switch cond epilogBlock cases
  cases <- mapM (genCase epilogBlock) cs'
  epilogBlock <- L.block `L.named` "epilog"
  pure ()
  where
    genCase epilog (SSwitchCase val stmts) = do
      l <- L.block `L.named` "case"
      mapM_ codegenStmt stmts
      mkTerminator $ L.br epilog
      pure (genConstant val, l)
codegenStmt (SDoWhileStmt body cond) = mdo
  L.br bodyBlock
  oldEnv <- get
  -- add labels to Env so break and continue point to the right label
  modify $ \env -> env {breakLabel = endBlock, continueLabel = bodyBlock}

  bodyBlock <- L.block `L.named` "body"
  codegenStmt body
  c <- codegenExpr cond
  mkTerminator $ L.condBr c bodyBlock endBlock

  endBlock <- L.block `L.named` "end"
  modify $ const oldEnv
  pure ()
codegenStmt (SForStmt init' cond inc body) = mdo
  L.br bodyBlock
  oldEnv <- get
  modify $ \env -> env {breakLabel = endBlock, continueLabel = incBlock}

  codegenStmt init'

  bodyBlock <- L.block `L.named` "body"
  codegenStmt body
  c <- codegenExpr cond
  mkTerminator $ L.condBr c incBlock endBlock

  -- for loops have a separate inc block to ensure that the loop increments when a continue statement is run 
  incBlock <- L.block `L.named` "inc"
  codegenStmt inc
  L.br bodyBlock

  endBlock <- L.block `L.named` "end"
  modify $ const oldEnv
codegenStmt (SVariableDeclStmt t ident expr) = do
  ty <- convType t
  addr <- L.alloca ty Nothing 0 -- allocate memory for variable
  modify $ \env -> env {operands = M.insert ident addr (operands env)}
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
  decayVal <- L.gep addr [L.int64 0, L.int64 0] -- return pointer to first item
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

-- checks if a block has a terminator before putting an instruction
mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr
