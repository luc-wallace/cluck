module Ast where

import Data.List (intercalate)
import Data.Text (Text)
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
  deriving (Eq)

instance Show Type where
  show Char = "char"
  show Bool = "bool"
  show Int = "int"
  show Float = "float"
  show Void = "void"
  show (Pointer t) = show t ++ "*"

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
  | BlockStmt [Stmt]
  | ExprStmt Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | DoWhileStmt Stmt Expr
  | ForStmt Expr Expr Expr Stmt
  | WhileStmt Expr Stmt
  | ReturnStmt (Maybe Expr)
  deriving (Show)

data Expr
  = CharLiteral Char
  | StringLiteral Text
  | IntLiteral Int
  | FloatLiteral Double
  | BoolLiteral Bool
  | VariableExpr Identifier
  | FunctionExpr Identifier [Expr]
  | UnaryOp Oprt Expr
  | BinaryOp Oprt Expr Expr
  | Cast Type Expr
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

genHeader :: Program -> String
genHeader (Program p) = intercalate "\n" $ map showDecl p
  where
    showDecl (FunctionDecl t i args _) =
      printf "%s %s(%s);" (show t) i (showArgs args)
    showDecl (VariableDecl t i _) =
      printf "%s %s;" (show t) i

    showArgs args =
      intercalate ", " $ map (\(ty, ident) -> printf "%s %s" (show ty) ident) args
