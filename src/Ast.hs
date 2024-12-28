module Ast where

import Data.Text (Text)

type Identifier = Text

newtype Program = Program [Decl] deriving (Show)

data Type
  = Char
  | Bool
  | Int
  | Float
  | Void
  | Pointer Type
  | Array Type (Maybe Int)
  deriving (Eq)

instance Show Type where
  show Char = "char"
  show Bool = "bool"
  show Int = "int"
  show Float = "float"
  show Void = "void"
  show (Pointer t) = show t ++ "*"
  show (Array t (Just size)) = show t ++ "[" ++ show size ++ "]"
  show (Array t Nothing) = show t ++ "[]"

data Decl
  = VariableDecl Type Identifier (Maybe Expr)
  | FunctionDecl Type Identifier [Arg] (Maybe Stmt)
  deriving (Show)

declTy :: Decl -> Type
declTy (VariableDecl t _ _) = t
declTy (FunctionDecl t _ _ _) = t

type Arg = (Type, Identifier)

data Stmt
  = VariableDeclStmt Type Identifier (Maybe Expr)
  | ArrayDeclStmt Type Identifier (Maybe Int) (Maybe [Expr])
  | BlockStmt [Stmt]
  | ExprStmt Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | DoWhileStmt Stmt Expr
  | ForStmt Expr Expr Expr Stmt
  | WhileStmt Expr Stmt
  | ReturnStmt (Maybe Expr)
  | BreakStmt
  | ContinueStmt
  deriving (Show)

data Expr
  = CharLiteral Char
  | StringLiteral Text
  | IntLiteral Int
  | FloatLiteral Double
  | BoolLiteral Bool
  | ArrayLiteral [Expr]
  | Null
  | VariableExpr Identifier
  | ArrayExpr Identifier Expr
  | FunctionExpr Identifier [Expr]
  | UnaryOp Oprt Expr
  | BinaryOp Oprt Expr Expr
  | Cast Type Expr
  | SizeOf Type
  deriving (Show, Eq)

data Oprt
  = Not
  | And
  | Or
  | Add
  | Mod
  | Sub
  | Mul
  | Div
  | Neg
  | EqTo
  | NtEqTo
  | Gt
  | GtOrEqTo
  | Lt
  | LtOrEqTo
  | Ref
  | Deref
  | Assign
  | Inc
  | Dec
  deriving (Eq)

instance Show Oprt where
  show Not = "!"
  show And = "&&"
  show Or = "||"
  show Add = "+"
  show Mod = "%"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Neg = "-"
  show EqTo = "=="
  show NtEqTo = "!="
  show Gt = ">"
  show GtOrEqTo = ">="
  show Lt = "<"
  show LtOrEqTo = "<="
  show Assign = "="
  show Ref = "&"
  show Deref = "*"
  show Inc = "++"
  show Dec = "--"
