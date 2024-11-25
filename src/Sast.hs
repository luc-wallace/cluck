module Sast where

import Ast
import Data.Text (Text)

newtype SProgram = SProgram [SDecl] deriving (Show)

data SDecl
  = SVariableDecl Type Identifier (Maybe SExpr)
  | SFunctionDecl Type Identifier [Arg] SStmt
  deriving (Show)

data SStmt
  = SVariableDeclStmt SDecl
  | SVariableAssignStmt Identifier SExpr
  | SBlockStmt [SStmt]
  | SExprStmt SExpr
  | SIfStmt SExpr SStmt SStmt
  | SReturnStmt (Maybe SExpr)
  deriving (Show)

type SExpr = (Type, SExpr')

data SExpr'
  = SCharLiteral Char
  | SStringLiteral Text
  | SIntLiteral Int
  | SFloatLiteral Double
  | SBoolLiteral Bool
  | SVariableExpr Identifier
  | SFunctionExpr Identifier [SExpr]
  | SUnaryOp Oprt SExpr
  | SBinaryOp Oprt SExpr SExpr
  deriving (Show, Eq)
