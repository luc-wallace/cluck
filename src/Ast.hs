module Ast where

import Data.Text (Text)
import Data.List (intercalate)
import Text.Printf (printf)

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

-- convert type to C syntax representation
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

type Arg = (Type, Identifier)

data SwitchCase = SwitchCase Expr [Stmt] deriving (Show)

data Stmt
  = VariableDeclStmt Type Identifier (Maybe Expr)
  | ArrayDeclStmt Type Identifier (Maybe Int) (Maybe [Expr])
  | BlockStmt [Stmt]
  | ExprStmt Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | SwitchStmt Expr [SwitchCase]
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
  | Null
  | VariableExpr Identifier
  | ArrayExpr Identifier Expr
  | FunctionExpr Identifier [Expr]
  | UnaryOp Oprt Expr
  | BinaryOp Oprt Expr Expr
  | Cast Type Expr
  | SizeOfType Type
  | SizeOfExpr Expr
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

-- convert operator to C syntax representation
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

-- generate C header file from AST
genHeader :: Program -> String
genHeader (Program p) = intercalate "\n\n" (map showDecl p) ++ "\n"
  where
    showDecl (FunctionDecl t i args _) =
      printf "%s %s(%s);" (show t) i (showArgs args)
    showDecl (VariableDecl t i _) =
      printf "%s %s;" (show t) i

    showArgs args =
      intercalate ", " $ map (\(ty, ident) -> printf "%s %s" (show ty) ident) args
