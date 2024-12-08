module Sast where

import Ast
import Data.Text (Text)

newtype SProgram = SProgram [SDecl] deriving (Show)

data SDecl
  = SVariableDecl Type Identifier (Maybe SExpr)
  | SFunctionDecl Type Identifier [Arg] SStmt
  deriving (Show)

data SStmt
  = SVariableDeclStmt Type Identifier (Maybe SExpr)
  | SBlockStmt [SStmt]
  | SExprStmt SExpr
  | SIfStmt SExpr SStmt SStmt
  | SDoWhileStmt SStmt SExpr
  | SReturnStmt (Maybe SExpr)
  deriving (Show)

type SExpr = (Type, SExpr')

data LVal
  = LVar Identifier
  | LDeref SExpr
  deriving (Show, Eq)

data SExpr'
  = SCharLiteral Char
  | SStringLiteral Text
  | SIntLiteral Int
  | SFloatLiteral Double
  | SBoolLiteral Bool
  | SFunctionExpr Identifier [SExpr]
  | SBinaryOp Oprt SExpr SExpr
  | SUnaryOp Oprt SExpr
  | SAssign LVal SExpr
  | SInc LVal
  | SDec LVal
  | LVal LVal
  | SRef LVal
  | SCast Type SExpr
  deriving (Show, Eq)
