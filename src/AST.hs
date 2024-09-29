module Ast where

type Identifier = String

data Type
  = Char
  | SignedChar
  | UnsignedChar
  | Short
  | UnsignedShort
  | Int
  | UnsignedInt
  | Long
  | UnsignedLong
  | Float
  | Double
  | LongDouble
  | Void
  | Pointer Type
  deriving (Show)

data Decl
  = VariableDecl Type Identifier (Maybe Expr)
  | FunctionDecl Type Identifier (Maybe Stmt)
  deriving (Show)

data Stmt
  = VariableDeclStmt Decl
  | VariableAssignStmt Identifier Expr
  | BlockStmt [Stmt]
  | ExprStmt Expr
  | IfStmt Expr Stmt
  | ReturnStmt Expr
  deriving (Show)

data Expr
  = String String
  | Number Double
  | VariableExpr Identifier
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Add Expr Expr
  | Mod Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  | EqTo Expr Expr
  | NtEqTo Expr Expr
  | Gt Expr Expr
  | GtOrEqTo Expr Expr
  | Lt Expr Expr
  | LtOrEqTo Expr Expr
  deriving (Show, Eq)
