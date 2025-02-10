module Semant where

import Ast
import Cfg
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import Sast
import qualified Text.Printf as Text

type Semant = ExceptT SemantError (State Env)

-- keeps track of program state while traversing it
data Env = Env
  { vars :: Map Identifier Decl,
    funcs :: Map Identifier Decl,
    curFunc :: (Identifier, Type),
    inLoop :: Bool
  }

data DeclKind
  = Function
  | Variable

dKind :: DeclKind -> Text
dKind Function = "function"
dKind Variable = "variable"

data SemantError
  = NameError DeclKind Identifier
  | RedefinitionError DeclKind Identifier
  | TypeError String Type Type
  | ArgumentError Identifier Int Int
  | BinaryOprtError Oprt Type Type
  | UnaryOprtError Oprt Type
  | VoidError DeclKind Identifier
  | ReturnError Identifier Type
  | ConstantError Identifier
  | CastError Type Type
  | LValError Oprt
  | BreakContError
  | ArrayDefError Identifier
  | ArrayInitError Identifier Int Int
  | ArrayTypeError Identifier Type
  | SwitchError

instance Show SemantError where
  show (NameError d ident) = Text.printf "error: %s '%s' is not defined" (dKind d) ident
  show (RedefinitionError d ident) = Text.printf "error: %s '%s' is redefined" (dKind d) ident
  show (TypeError msg t1 t2) = Text.printf "error: %s\n       expected %s but got %s" msg (show t1) (show t2)
  show (ArgumentError ident e g) = Text.printf "error: expected %d arguments in call to '%s(...)' but got %d" e ident g
  show (BinaryOprtError op t1 t2) = Text.printf "error: operation '%s' is not possible for types %s and %s" (show op) (show t1) (show t2)
  show (UnaryOprtError op t) = Text.printf "error: operation '%s' is not possible for type %s" (show op) (show t)
  show (VoidError d ident) = case d of
    Function -> Text.printf "error: unexpected return statement in function '%s(...)' of type void" ident
    Variable -> Text.printf "error: variable '%s' cannot have type void" ident
  show (ReturnError ident Void) = Text.printf "error: void function '%s(...)' cannot return a value" ident
  show (ReturnError ident _) = Text.printf "error: non-void function '%s(...)' does not return a value in all control paths" ident
  show (ConstantError ident) = Text.printf "error: global variable '%s' must have a constant value" ident
  show (CastError t1 t2) = Text.printf "error: unable to type cast %s to %s" (show t2) (show t1)
  show (LValError op) = Text.printf "error: expected lval as argument to operator '%s'" (show op)
  show BreakContError = Text.printf "error: cannot use break/continue statement outside of a loop"
  show (ArrayDefError ident) = Text.printf "error: array '%s[]' declared without size" ident
  show (ArrayInitError ident size vals) = Text.printf "error: array '%s[]' of size %d initialised with %d values" ident size vals
  show (ArrayTypeError ident t) = Text.printf "error: array '%s[]' initialised with non-%s value" ident (show t)
  show SwitchError = Text.printf "error: switch case expression must have a constant value"

isNumeric :: Type -> Bool
isNumeric t = t `elem` [Int, Float, Char, Bool]

isLogical :: Oprt -> Bool
isLogical op = op `elem` [Not, And, Or, EqTo, NtEqTo, Gt, GtOrEqTo, Lt, LtOrEqTo]

isPointer :: Type -> Bool
isPointer (Pointer _) = True
isPointer _ = False

-- returns the type that a pointer type points to
unwrapPointer :: Type -> Semant Type
unwrapPointer (Pointer t) = pure t
unwrapPointer t = throwError $ TypeError "couldn't unwrap pointer type" (Pointer t) t

-- checks if an expression is of constant type
isConstant :: Expr -> Bool
isConstant (IntLiteral _) = True
isConstant (BoolLiteral _) = True
isConstant (FloatLiteral _) = True
isConstant (CharLiteral _) = True
isConstant _ = False

analyseProgram :: Program -> Either SemantError SProgram
analyseProgram (Program decls) =
  evalState (runExceptT (SProgram <$> mapM analyseDecl decls)) baseEnv
  where
    -- built-in functions added to Env from beginning so they do not raise "function not defined" errors
    builtIns =
      [ ("malloc", FunctionDecl (Pointer Void) "malloc" [(Int, "")] Nothing),
        ("free", FunctionDecl Void "free" [(Pointer Void, "")] Nothing),
        ("scanf", FunctionDecl Int "scanf" [(Pointer Char, ""), (Pointer Void, "")] Nothing),
        ("printf", FunctionDecl Int "printf" [(Pointer Char, "")] Nothing),
        ("sqrt", FunctionDecl Float "sqrt" [(Float, "")] Nothing),
        ("pow", FunctionDecl Float "pow" [(Float, ""), (Float, "")] Nothing)
      ]
    baseEnv = Env {vars = M.empty, funcs = M.fromList builtIns, curFunc = ("", Void), inLoop = False}

analyseDecl :: Decl -> Semant SDecl
analyseDecl d@(VariableDecl t1 ident expr) = do
  -- variables cannot have the type void
  when (t1 == Void) $ throwError $ VoidError Variable ident
  vars' <- gets vars
  -- variables cannot be redeclared
  when (isRedefined vars' ident) $ throwError $ RedefinitionError Variable ident

  sDecl <- case expr of
    Nothing -> pure $ SVariableDecl t1 ident Nothing
    (Just e) -> do
      -- variable declarations in the global scope can only have a constant value
      unless (isConstant e) $ throwError $ ConstantError ident
      sExpr@(t2, _) <- analyseExpr e
      if t1 == t2
        then pure $ SVariableDecl t1 ident (Just sExpr)
        else throwError $ TypeError (Text.printf "variable '%s' initialised with incorrect type" ident) t1 t2

  -- add variable to Env
  modify $ \env -> env {vars = M.insert ident d vars'}
  pure sDecl
analyseDecl d@(FunctionDecl t ident args stmt) = do
  vars' <- gets vars
  funcs' <- gets funcs
  modify $ \env -> env {funcs = M.insert ident d funcs', vars = insertArgs args vars', curFunc = (ident, t)}

  -- functions cannot be redeclared
  when (isRedefined funcs' ident) $
    throwError $ RedefinitionError Function ident

  sDecl <- case stmt of
    Nothing -> pure $ SFunctionDecl t ident args Nothing
    Just s@(BlockStmt stmts) -> do
      sStmt <- analyseStmt s
      -- use control flow graph to ensure that the function returns in all cases - unless it is void
      unless (t == Void || validate (genCFG stmts)) $ throwError $ ReturnError ident t

      pure $ SFunctionDecl t ident args (Just sStmt)
    _ -> error "error: parse failed"

  modify $ \env -> env {vars = vars'}
  pure sDecl
  where
    insertArgs :: [Arg] -> Map Identifier Decl -> Map Identifier Decl
    insertArgs args' m =
      -- args are added to the Env to function as variable declarations within the function body
      foldl (\m' (t', ident') -> M.insert ident' (VariableDecl t' ident' Nothing) m') m args'

isRedefined :: Map Identifier Decl -> Identifier -> Bool
isRedefined m ident = case M.lookup ident m of
  Just (FunctionDecl _ _ _ (Just _)) -> True
  Just (VariableDecl _ _ (Just _)) -> True
  _ -> False

analyseStmt :: Stmt -> Semant SStmt
analyseStmt (BlockStmt stmts) = do
  oldState <- get
  -- recursively analyses all statements in the array
  sstmts <- mapM analyseStmt stmts
  put oldState
  pure $ SBlockStmt sstmts
analyseStmt (VariableDeclStmt t1 ident expr) = do
  when (t1 == Void) $ throwError $ VoidError Variable ident
  vars' <- gets vars
  when (isRedefined vars' ident) $ throwError $ RedefinitionError Variable ident
  modify $ \env -> env {vars = M.insert ident (VariableDecl t1 ident expr) vars'}
  case expr of
    Just e -> do
      sExpr@(t2, _) <- analyseExpr e
      if t1 == t2
        then do
          modify $ \env -> env {vars = M.insert ident (VariableDecl t1 ident expr) vars'}
          pure $ SVariableDeclStmt t1 ident (Just sExpr)
        else throwError $ TypeError (Text.printf "variable '%s' initialised with incorrect type" ident) t1 t2
    Nothing -> pure $ SVariableDeclStmt t1 ident Nothing
analyseStmt (ArrayDeclStmt t1 ident size init') = do
  vars' <- gets vars
  when (isRedefined vars' ident) $ throwError $ RedefinitionError Variable ident
  (size', items) <- case init' of
    Nothing -> do
      -- if the array was not initialised with values
      when (isNothing size) $ throwError $ ArrayDefError ident
      pure (fromJust size, Nothing)
    Just arr -> do
      sArr <- mapM analyseExpr arr
      -- all values must be the same type as the array
      unless (all ((== t1) . fst) sArr) $ throwError $ ArrayTypeError ident t1
      case size of
        Nothing -> pure (length sArr, Just sArr)
        Just n -> do
          -- array must be initialised with the same number of values as its size
          when (n /= length sArr) $ throwError $ ArrayInitError ident n (length sArr)
          pure (n, Just sArr)
  modify $ \env -> env {vars = M.insert ident (VariableDecl (Pointer t1) ident Nothing) vars'}
  pure $ SArrayDeclStmt t1 ident size' items
analyseStmt (ExprStmt expr) = do
  sExpr <- analyseExpr expr
  pure $ SExprStmt sExpr
analyseStmt (IfStmt expr t e) = do
  sExpr@(ty, _) <- analyseExpr expr
  -- if statement conditions must be a boolean
  unless (ty == Bool) $ throwError $ TypeError "invalid if-statement condition" Bool ty
  sThen <- analyseStmt t
  case e of
    Nothing -> pure $ SIfStmt sExpr sThen (SBlockStmt [])
    Just stmt -> do
      s <- analyseStmt stmt
      pure $ SIfStmt sExpr sThen s
analyseStmt (SwitchStmt e cases) = do
  sExpr@(t, _) <- analyseExpr e
  sCases <- mapM (analyseCase t) cases
  pure $ SSwitchStmt sExpr sCases
  where
    analyseCase :: Type -> SwitchCase -> Semant SSwitchCase
    analyseCase t (SwitchCase e' stmts) = do
      sExpr'@(t', _) <- analyseExpr e'
      -- switch cases must be of the same type as the original switch expression
      when (t /= t') $ throwError $ TypeError "invalid switch case" t t'
      unless (isConstant e') $ throwError SwitchError
      sstmts <- mapM analyseStmt stmts
      pure $ SSwitchCase sExpr' sstmts
analyseStmt (ReturnStmt e) = do
  (ident, rett) <- gets curFunc
  case (e, rett) of
    (Nothing, Void) -> pure $ SReturnStmt Nothing
    -- a non-void function must return a value
    (Nothing, _) -> throwError $ ReturnError ident rett
    -- a void function cannot return a value
    (Just _, Void) -> throwError $ VoidError Function ident
    (Just expr, _) -> do
      sExpr@(t, _) <- analyseExpr expr
      -- a return statement must return a value of the same type as the function definition
      if rett == t
        then pure $ SReturnStmt (Just sExpr)
        else throwError $ TypeError (Text.printf "function '%s(...)' has invalid return value" ident) rett t
analyseStmt (DoWhileStmt stmt cond) = do
  inl <- gets inLoop
  -- so compiler remembers we are currently in a loop - break/continue are accessible
  unless inl $ modify $ \env -> env {inLoop = True}

  sStmt <- analyseStmt stmt
  sCond@(t, _) <- analyseExpr cond

  -- while loop condition must be a boolean
  unless (t == Bool) $ throwError $ TypeError "invalid while-loop condition" Bool t

  -- we are no longer in a loop
  unless inl $ modify $ \env -> env {inLoop = False}
  pure $ SDoWhileStmt sStmt sCond
analyseStmt (ForStmt e1 e2 e3 stmt) = do
  inl <- gets inLoop
  unless inl $ modify $ \env -> env {inLoop = True}

  init' <- analyseExpr e1
  cond@(t1, _) <- analyseExpr e2
  inc <- analyseExpr e3
  sstmt <- analyseStmt stmt

  unless (t1 == Bool) $ throwError $ TypeError "invalid for-loop condition" Bool t1
  unless inl $ modify $ \env -> env {inLoop = False}
  pure $ SForStmt (SExprStmt init') cond (SExprStmt inc) sstmt
-- while loops can be transformed into do-while loops
analyseStmt (WhileStmt cond stmt) = analyseStmt $ IfStmt cond (DoWhileStmt stmt cond) Nothing
analyseStmt BreakStmt = do
  inl <- gets inLoop
  -- break can only be used inside a loop
  unless inl $ throwError BreakContError
  pure SBreakStmt
analyseStmt ContinueStmt = do
  inl <- gets inLoop
  -- continue can only be used inside a loop
  unless inl $ throwError BreakContError
  pure SContinueStmt

analyseExpr :: Expr -> Semant SExpr
-- literals do not need to be analysed
analyseExpr (IntLiteral n) = pure (Int, SIntLiteral n)
analyseExpr (FloatLiteral n) = pure (Float, SFloatLiteral n)
analyseExpr (CharLiteral c) = pure (Char, SCharLiteral c)
analyseExpr (BoolLiteral b) = pure (Bool, SBoolLiteral b)
analyseExpr Null = pure (Pointer Void, SNull)
analyseExpr (StringLiteral s) = pure (Pointer Char, SStringLiteral s)
analyseExpr (VariableExpr ident) = do
  vars' <- gets vars
  -- check if variable has been defined
  case M.lookup ident vars' of
    Nothing -> throwError $ NameError Variable ident
    Just (VariableDecl t _ _) -> pure (t, LVal (LVar ident))
    _ -> error "error: invalid env state"
analyseExpr (ArrayExpr ident index) = do
  vars' <- gets vars
  sIndex@(ty, _) <- analyseExpr index
  -- arrays can only be indexed using an integer
  unless (ty == Int) $ throwError $ TypeError (Text.printf "array '%s[]' indexed with invalid type" ident) Int ty

  -- check if array exists
  case M.lookup ident vars' of
    Nothing -> throwError $ NameError Variable ident
    Just (VariableDecl (Pointer t) _ _) -> do
      pure (t, LVal (LArray ident sIndex))
    _ -> throwError $ NameError Variable ident
-- free, scanf and prinf are manually implemented as they have special function signatures
analyseExpr (FunctionExpr "free" args) = do
  unless (length args == 1) $ throwError $ ArgumentError "free" 1 (length args)
  sAddr@(t, _) <- analyseExpr $ head args
  _ <- unwrapPointer t
  pure (Void, SFunctionExpr "free" [(Pointer t, SCast (Pointer Void) sAddr)])
analyseExpr (FunctionExpr "scanf" args) = do
  unless (length args == 2) $ throwError $ ArgumentError "scanf" 2 (length args)
  sArgs <- mapM analyseExpr args
  let str@(t1, _) = head sArgs
  let addr@(t2, _) = last sArgs
  unless (t1 == Pointer Char) $ throwError $ TypeError "expected format string in arguments to scanf" (Pointer Char) t1
  unless (isPointer t2) $ throwError $ TypeError "expected pointer in arguments to scanf" (Pointer t2) t2
  pure (Int, SFunctionExpr "scanf" [str, addr])
analyseExpr (FunctionExpr "printf" args) = do
  when (null args) $ throwError $ ArgumentError "printf" 1 0
  sArgs <- mapM analyseExpr args
  let (t, _) = head sArgs
  unless (t == Pointer Char) $ throwError $ TypeError "expected format string in arguments to printf" (Pointer Char) t
  pure (Int, SFunctionExpr "printf" sArgs)
analyseExpr (FunctionExpr ident args) = do
  funcs' <- gets funcs
  -- check if function exists
  case M.lookup ident funcs' of
    Nothing -> throwError $ NameError Function ident
    Just (FunctionDecl t _ args' _) ->
      let expt = length args'
          acc = length args
       in if expt - acc /= 0
            then -- function call must have the same number of args as the original function definition
              throwError $ ArgumentError ident expt acc
            else do
              sArgs <- mapM analyseArg $ zip [1 ..] $ zip args' args
              pure (t, SFunctionExpr ident sArgs)
    _ -> error "error: invalid env state"
  where
    analyseArg :: (Int, (Arg, Expr)) -> Semant SExpr
    analyseArg (n, ((t1, _), expr)) = do
      e@(t2, _) <- analyseExpr expr
      case (t1, t2) of
        -- check if arg has the same type as the function definition
        (Array t1' _, Array t2' _) ->
          if t1' == t2'
            then pure e
            else throwError $ TypeError (Text.printf "arg number %d has invalid type in call to '%s(...)'" n ident) t1 t2
        _ ->
          if t1 == t2
            then pure e
            else throwError $ TypeError (Text.printf "arg number %d has invalid type in call to '%s(...)'" n ident) t1 t2
analyseExpr (BinaryOp Assign e1 e2) = do
  (t1, e) <- analyseExpr e1
  rhs@(t2, _) <- analyseExpr e2
  -- assignment cannot occur between mismatched types
  unless (t1 == t2) $ throwError $ TypeError "cannot assign to mismatched types" t1 t2
  case e of
    LVal l -> pure (t1, SAssign l rhs)
    _ -> throwError $ LValError Assign
analyseExpr (BinaryOp op e1 e2) = do
  lhs@(t1, _) <- analyseExpr e1
  rhs@(t2, _) <- analyseExpr e2
  let sbinop = SBinaryOp op lhs rhs

  -- check operator and operand type pairings
  case op of
    Add -> case (t1, t2) of
      (Int, Int) -> pure (Int, sbinop)
      (Float, Float) -> pure (Float, sbinop)
      (Pointer t, Int) -> pure (Pointer t, sbinop)
      (Int, Pointer t) -> pure (Pointer t, sbinop)
      _ -> throwError $ BinaryOprtError op t1 t2
    Sub -> case (t1, t2) of
      (Int, Int) -> pure (Int, sbinop)
      (Float, Float) -> pure (Float, sbinop)
      (Pointer t, Int) -> pure (Pointer t, sbinop)
      _ -> throwError $ BinaryOprtError op t1 t2
    _ -> do
      -- logical operators (apart from equality) can only be performed on numeric values
      unless (t1 == t2 && (op `elem` [EqTo, NtEqTo] || isNumeric t1)) $ throwError $ BinaryOprtError op t1 t2

      -- logical operators always produce a boolean value
      if isLogical op
        then pure (Bool, sbinop)
        else pure (t1, sbinop)
analyseExpr (UnaryOp op expr) = do
  sExpr@(t, e) <- analyseExpr expr
  case op of
    Neg -> if isNumeric t && t /= Bool then pure (t, SUnaryOp Neg sExpr) else throwError $ UnaryOprtError op t
    Not -> if t == Bool then pure (Bool, SUnaryOp Not sExpr) else throwError $ UnaryOprtError op t
    Inc -> case e of -- only LVals can be incremented/decremented
      LVal l -> pure (t, SInc l)
      _ -> throwError $ LValError op
    Dec -> case e of
      LVal l -> pure (t, SDec l)
      _ -> throwError $ LValError op
    Ref -> do
      -- only an LVal can be referenced
      case e of
        LVal l -> pure (Pointer t, SRef l)
        _ -> throwError $ LValError Ref
    Deref -> do
      -- only pointer types can be dereferenced
      ty <- unwrapPointer t
      pure (ty, LVal $ LDeref sExpr)
    _ -> error $ "error: invalid unary operator " ++ show op
analyseExpr (Cast t1 expr) = do
  sExpr@(t2, _) <- analyseExpr expr
  if t1 == t2
    then pure sExpr -- no need to cast if cast destination and operand are the same type
    -- check if cast destination and operand type fail into one of the valid pairings
    else case (t1, t2) of
      (Pointer t, Pointer _) -> pure (Pointer t, SCast (Pointer t) sExpr)
      (Int, Pointer _) -> pure (Int, SCast Int sExpr)
      (Pointer t, Int) -> pure (Pointer t, SCast (Pointer t) sExpr)
      (Float, Int) -> pure (Float, SCast Float sExpr)
      (Int, Float) -> pure (Int, SCast Int sExpr)
      (Int, Char) -> pure (Int, SCast Int sExpr)
      (Char, Int) -> pure (Char, SCast Char sExpr)
      _ -> throwError $ CastError t1 t2
analyseExpr (SizeOfType t) = pure (Int, SSizeOf t)
analyseExpr (SizeOfExpr e) = do
  (t, _) <- analyseExpr e
  pure (Int, SSizeOf t)
analyseExpr expr = error $ "error: expression not implemented " ++ show expr
