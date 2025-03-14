module Sast where

import Ast
import Data.Text (Text)

newtype SProgram = SProgram [SDecl] deriving (Show)

data SDecl
  = SVariableDecl Type Identifier (Maybe SExpr)
  | SFunctionDecl Type Identifier [Arg] (Maybe SStmt)
  deriving (Show)

data SSwitchCase = SSwitchCase SExpr [SStmt] deriving (Show)

data SStmt
  = SVariableDeclStmt Type Identifier (Maybe SExpr)
  | SArrayDeclStmt Type Identifier Int (Maybe [SExpr])
  | SBlockStmt [SStmt]
  | SExprStmt SExpr
  | SIfStmt SExpr SStmt SStmt
  | SSwitchStmt SExpr [SSwitchCase]
  | SDoWhileStmt SStmt SExpr
  | SForStmt SStmt SExpr SStmt SStmt
  | SReturnStmt (Maybe SExpr)
  | SBreakStmt
  | SContinueStmt
  deriving (Show)

-- defining the SExpr enables type information to be annotated onto the AST, enabling type checking
type SExpr = (Type, SExpr')

-- an LVal is anything that can be on the left-hand side of an assignment expression
data LVal
  = LVar Identifier
  | LArray Identifier SExpr
  | LDeref SExpr
  deriving (Show, Eq)

data SExpr'
  = SCharLiteral Char
  | SStringLiteral Text
  | SIntLiteral Int
  | SFloatLiteral Double
  | SBoolLiteral Bool
  | SNull
  | SFunctionExpr Identifier [SExpr]
  | SBinaryOp Oprt SExpr SExpr
  | SUnaryOp Oprt SExpr
  | SAssign LVal SExpr
  | SInc LVal
  | SDec LVal
  | LVal LVal
  | SRef LVal
  | SCast Type SExpr
  | SSizeOf Type
  deriving (Show, Eq)
