module Semant where

import Ast
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Text.Printf as Text

-- TODO: check invalid returns

newtype SProgram = SProgram [SDecl] deriving (Show)

data Env = Env
  { vars :: Map Identifier Decl,
    funcs :: Map Identifier Decl,
    curFunc :: (Identifier, Type)
  }

type Semant = ExceptT SemantError (State Env)

data SDecl
  = SVariableDecl Type Identifier (Maybe SExpr)
  | SFunctionDecl Type Identifier [Arg] (Maybe SStmt)
  deriving (Show)

data SStmt
  = SVariableDeclStmt SDecl
  | SVariableAssignStmt Identifier SExpr
  | SBlockStmt [SStmt]
  | SExprStmt SExpr
  | SIfStmt SExpr SStmt
  | SReturnStmt SExpr
  deriving (Show)

type SExpr = (Type, SExpr')

data SExpr'
  = SCharLiteral Char
  | SStringLiteral Text
  | SNumberLiteral Double
  | SBoolLiteral Bool
  | SVariableExpr Identifier
  | SFunctionExpr Identifier [SExpr]
  | SUnaryOp Oprt SExpr
  | SBinaryOp Oprt SExpr SExpr
  deriving (Show, Eq)

data DeclKind
  = Function
  | Variable

dKind :: DeclKind -> Text
dKind Function = "function"
dKind Variable = "variable"

data SemantError
  = NameError DeclKind Identifier
  | RedefinitionError DeclKind Identifier
  | TypeError Type Type
  | ArgumentError Identifier Int Int
  | BinaryOprtError Oprt Type Type
  | UnaryOprtError Oprt Type
  | VoidError DeclKind Identifier

instance Show SemantError where
  show (NameError d ident) = Text.printf "error: %s %s is not defined" (dKind d) ident
  show (RedefinitionError d ident) = Text.printf "error: %s %s is redefined" (dKind d) ident
  show (TypeError t1 t2) = Text.printf "error: expected %s but got %s" (show t1) (show t2)
  show (ArgumentError ident e g) = Text.printf "error: expected %d arguments in call to %s(...) but got %d" e ident g
  show (BinaryOprtError op t1 t2) = Text.printf "error: operation \"%s\" is not possible for types %s and %s" (show op) (show t1) (show t2)
  show (UnaryOprtError op t) = Text.printf "error: no instance of %s for operator %s" (show t) (show op)
  show (VoidError d ident) = case d of
    Function -> Text.printf "error: unexpected return statement in function %s(...) of type void" ident
    Variable -> Text.printf "error: variable cannot have type void"

isNumeric :: Type -> Bool
isNumeric t = t `elem` [Int, Float, Char, Bool]

isLogical :: Oprt -> Bool
isLogical op = op `elem` [Not, And, Or, EqTo, NtEqTo, Gt, GtOrEqTo, Lt, LtOrEqTo]

analyseProgram :: Program -> Either SemantError SProgram
analyseProgram (Program decls) =
  evalState (runExceptT (SProgram <$> mapM analyseDecl decls)) baseEnv
  where
    baseEnv = Env {vars = M.empty, funcs = M.empty, curFunc = ("", Void)}

analyseDecl :: Decl -> Semant SDecl
analyseDecl d@(VariableDecl t1 ident expr) = do
  when (t1 == Void) $ throwError $ VoidError Variable ident
  vars' <- gets vars
  when (isRedefined vars' ident) $ throwError $ RedefinitionError Variable ident

  sDecl <- case expr of
    Nothing -> pure $ SVariableDecl t1 ident Nothing
    (Just e) -> do
      sExpr@(t2, _) <- analyseExpr e
      if t1 == t2
        then pure $ SVariableDecl t1 ident (Just sExpr)
        else throwError $ TypeError t1 t2

  modify $ \env -> env {vars = M.insert ident d vars'}
  pure sDecl
analyseDecl d@(FunctionDecl t ident args stmt) = do
  vars' <- gets vars
  funcs' <- gets funcs
  modify $ \env -> env {funcs = M.insert ident d funcs', vars = insertArgs args vars', curFunc = (ident, t)}

  when (isRedefined funcs' ident) $
    throwError $ RedefinitionError Function ident

  sDecl <- case stmt of
    Nothing -> pure $ SFunctionDecl t ident args Nothing
    Just s -> do
      sStmt <- analyseStmt s
      pure $ SFunctionDecl t ident args (Just sStmt)

  modify $ \env -> env {vars = vars'}
  pure sDecl
  where
    insertArgs :: [Arg] -> Map Identifier Decl -> Map Identifier Decl
    insertArgs args' m =
      foldl (\m' (t', ident') -> M.insert ident' (VariableDecl t' ident' Nothing) m') m args'

isRedefined :: Map Identifier Decl -> Identifier -> Bool
isRedefined m ident = case M.lookup ident m of
  Just (FunctionDecl _ _ _ (Just _)) -> True
  Just (VariableDecl _ _ (Just _)) -> True
  _ -> False

analyseStmt :: Stmt -> Semant SStmt
analyseStmt (BlockStmt stmts) = do
  oldState <- get
  sstmts <- mapM analyseStmt stmts
  put oldState
  pure $ SBlockStmt sstmts
analyseStmt (VariableDeclStmt d@(VariableDecl t1 ident expr)) = do
  when (t1 == Void) $ throwError $ VoidError Variable ident
  vars' <- gets vars
  case M.lookup ident vars' of
    Nothing -> case expr of
      Just e -> do
        sExpr@(t2, _) <- analyseExpr e
        if t1 == t2
          then do
            modify $ \env -> env {vars = M.insert ident d vars'}
            pure $ SVariableDeclStmt (SVariableDecl t1 ident (Just sExpr))
          else throwError $ TypeError t1 t2
      Nothing -> pure $ SVariableDeclStmt (SVariableDecl t1 ident Nothing)
    Just _ -> throwError $ RedefinitionError Variable ident
analyseStmt (VariableAssignStmt ident expr) = do
  vars' <- gets vars
  case M.lookup ident vars' of
    Nothing -> throwError $ NameError Variable ident
    Just (VariableDecl t1 _ _) -> do
      sExpr@(t2, _) <- analyseExpr expr
      if t1 == t2
        then pure $ SVariableAssignStmt ident sExpr
        else throwError $ TypeError t1 t2
    _ -> error "parse failed"
analyseStmt (ExprStmt expr) = do
  sExpr <- analyseExpr expr
  pure $ SExprStmt sExpr
analyseStmt (IfStmt expr stmt) = do
  sExpr <- analyseExpr expr
  sStmt <- analyseStmt stmt
  pure $ SIfStmt sExpr sStmt
analyseStmt (ReturnStmt expr) = do
  (ident, rett) <- gets curFunc
  sExpr@(t, _) <- analyseExpr expr

  when (rett == Void) $ throwError $ VoidError Function ident
  if rett == t
    then pure $ SReturnStmt sExpr
    else throwError $ TypeError rett t
analyseStmt _ = error "error: parse failed"

analyseExpr :: Expr -> Semant SExpr
analyseExpr (NumberLiteral n) = pure (Int, SNumberLiteral n)
analyseExpr (CharLiteral c) = pure (Char, SCharLiteral c)
analyseExpr (BoolLiteral b) = pure (Bool, SBoolLiteral b)
-- analyseExpr (StringLiteral s) = pure (String, SCharLiteral s)
analyseExpr (VariableExpr ident) = do
  vars' <- gets vars
  case M.lookup ident vars' of
    Nothing -> throwError $ NameError Variable ident
    Just d -> pure (declTy d, SVariableExpr ident)
analyseExpr (FunctionExpr ident args) = do
  funcs' <- gets funcs
  case M.lookup ident funcs' of
    Nothing -> throwError $ NameError Function ident
    Just (FunctionDecl t _ args' _) ->
      let expt = length args'
          acc = length args
       in if expt - acc /= 0
            then throwError $ ArgumentError ident expt acc
            else do
              sArgs <- mapM analyseArg $ zip args' args
              pure (t, SFunctionExpr ident sArgs)
    _ -> error "error: invalid env state"
  where
    analyseArg :: (Arg, Expr) -> Semant SExpr
    analyseArg ((t1, _), expr) = do
      e@(t2, _) <- analyseExpr expr
      if t1 == t2
        then pure e
        else throwError $ TypeError t1 t2
analyseExpr (BinaryOp op e1 e2) = do
  lhs@(t1, _) <- analyseExpr e1
  rhs@(t2, _) <- analyseExpr e2

  unless (t1 == t2 && isNumeric t1) $ throwError $ BinaryOprtError op t1 t2
  if isLogical op
    then pure (Bool, SBinaryOp op lhs rhs)
    else pure (t1, SBinaryOp op lhs rhs)
analyseExpr (UnaryOp op expr) = do
  sExpr@(t, _) <- analyseExpr expr
  case op of
    Not -> if t == Bool then pure sExpr else throwError $ UnaryOprtError op t
    Neg -> if isNumeric t then pure sExpr else throwError $ UnaryOprtError op t
    _ -> error $ "error: invalid unary operator " ++ show op
analyseExpr expr = error $ "error: expression not implemented " ++ show expr
