module Semant where

import Ast
import Control.Monad (foldM)
import Control.Monad.Combinators ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Text.Printf (printf)

newtype SProgram = SProgram [SDecl] deriving (Show)

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
  | SVariableExpr Identifier
  | SFunctionExpr Identifier [SExpr]
  | SUnaryOp Oprt SExpr
  | SBinaryOp Oprt SExpr SExpr
  deriving (Show, Eq)

data SemanticError
  = NameError Identifier
  | TypeError Type Type

instance Show SemanticError where
  show (NameError i) = printf "name error\n%s not defined" i
  show (TypeError expected actual) = printf "type error\nexpected: %s\ngot: %s" (typeStr expected) (typeStr actual)

type SymbolTable = Map Identifier Type

type ScopeStack = [SymbolTable]

analyseProgram :: ScopeStack -> Program -> Either SemanticError SProgram
analyseProgram scope (Program decls) = do
  (_, sDecls) <- foldM accum (scope, []) decls
  return (SProgram sDecls)
  where
    accum :: (ScopeStack, [SDecl]) -> Decl -> Either SemanticError (ScopeStack, [SDecl])
    accum (s, sDecls) decl = do
      sDecl <- analyseDecl s decl

      let (ident, t) = case sDecl of
            SVariableDecl t ident _ -> (ident, t)
            SFunctionDecl t ident _ _ -> (ident, t)

      let newScope = pushScope scope ident t
      return (newScope, sDecls ++ [sDecl])

analyseDecl :: ScopeStack -> Decl -> Either SemanticError SDecl
analyseDecl scope (VariableDecl t ident val) = case val of
  Nothing -> Right (SVariableDecl t ident Nothing)
  Just expr -> do
    (et, sExpr) <- analyseExpr scope expr
    if et == t -- TODO: implicit type cast with numerical types
      then Right (SVariableDecl t ident (Just (et, sExpr)))
      else Left (TypeError t et)
analyseDecl scope (FunctionDecl t ident args body) = do
  let newScope = Map.fromList (map (\(a, b) -> (b, a)) args) : pushScope scope ident t
  case body of
    Nothing -> Right (SFunctionDecl t ident args Nothing)
    Just stmt -> do
      sStmt <- analyseStmt newScope stmt
      return $ SFunctionDecl t ident args $ Just sStmt

pushScope :: ScopeStack -> Identifier -> Type -> ScopeStack
pushScope scope ident t = Map.insert ident t (head scope) : tail scope

lookupScope :: Identifier -> ScopeStack -> Maybe Type
lookupScope ident = foldr (\scope acc -> acc <|> Map.lookup ident scope) Nothing

-- annotate expressions with type information
analyseExpr :: ScopeStack -> Expr -> Either SemanticError SExpr
analyseExpr scope (UnaryOp _ expr) = analyseExpr scope expr
analyseExpr scope (BinaryOp op e1 e2) = do
  expr1@(t1, _) <- analyseExpr scope e1
  expr2@(t2, _) <- analyseExpr scope e2
  if t1 == t2 then Right (t1, SBinaryOp op expr1 expr2) else Left (TypeError t1 t2)
analyseExpr _ (NumberLiteral n) = Right (Int, SNumberLiteral n)
analyseExpr _ (CharLiteral c) = Right (Char, SCharLiteral c)
analyseExpr scope (VariableExpr ident) =
  case lookupScope ident scope of
    Just t -> Right (t, SVariableExpr ident)
    Nothing -> Left (NameError ident)
analyseExpr _ _ = undefined

analyseStmt :: ScopeStack -> Stmt -> Either SemanticError SStmt
analyseStmt scope (BlockStmt stmts) = do
  (_, sStmts) <- foldM accum (Map.empty : scope, []) stmts
  return (SBlockStmt sStmts)
  where
    accum :: (ScopeStack, [SStmt]) -> Stmt -> Either SemanticError (ScopeStack, [SStmt])
    accum (s, sStmts) stmt = do
      sStmt <- analyseStmt s stmt

      let newScope = case sStmt of
            SVariableDeclStmt (SVariableDecl t ident _) -> pushScope scope ident t
            _ -> scope

      return (newScope, sStmts ++ [sStmt])
analyseStmt scope (IfStmt expr stmt) = do
  sExpr <- analyseExpr scope expr
  sStmt <- analyseStmt scope stmt
  return $ SIfStmt sExpr sStmt
analyseStmt scope (ExprStmt expr) = do
  sExpr <- analyseExpr scope expr
  return $ SExprStmt sExpr

-- TODO: check return type of function
analyseStmt scope (ReturnStmt expr) = do
  sExpr <- analyseExpr scope expr
  return $ SReturnStmt sExpr
analyseStmt scope (VariableDeclStmt (VariableDecl t ident e)) = do
  case e of
    Nothing -> Right $ SVariableDeclStmt (SVariableDecl t ident Nothing)
    (Just expr) -> do
      sExpr@(et, _) <- analyseExpr scope expr
      if et == t
        then Right $ SVariableDeclStmt (SVariableDecl t ident $ Just sExpr)
        else Left $ TypeError et t
analyseStmt scope (VariableAssignStmt ident expr) = do
  let mt = lookupScope ident scope
  case mt of
    Just t -> do
      sExpr@(et, _) <- analyseExpr scope expr
      if et == t
        then Right $ SVariableAssignStmt ident sExpr
        else Left $ TypeError et t
    Nothing -> Left $ NameError ident
analyseStmt _ stmt = error $ "invalid statement: " ++ show stmt
