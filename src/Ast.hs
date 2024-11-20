module Ast where

import Data.List (intercalate)
import Data.Text (Text, append)
import Text.Printf (printf)

type Identifier = Text

newtype Program = Program [Decl] deriving (Show)

data Type
  = Char
  | SignedChar
  | UnsignedChar
  | Short
  | Bool
  | UnsignedShort
  | Int
  | UnsignedInt
  | Long
  | UnsignedLong
  | Float
  | Double
  | LongDouble
  | Void
  | Custom Identifier
  | Pointer Type
  deriving (Eq)

instance Show Type where
  show Char = "char"
  show Bool = "bool"
  show SignedChar = "signed char"
  show UnsignedChar = "unsigned char"
  show Short = "short"
  show UnsignedShort = "unsigned short"
  show Int = "int"
  show UnsignedInt = "unsigned int"
  show Long = "long"
  show UnsignedLong = "unsigned long"
  show Float = "float"
  show Double = "double"
  show LongDouble = "long double"
  show Void = "void"
  show (Custom t) = show t
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
  = VariableDeclStmt Decl
  | VariableAssignStmt Identifier Expr
  | BlockStmt [Stmt]
  | ExprStmt Expr
  | IfStmt Expr Stmt
  | ReturnStmt Expr
  deriving (Show)

data Expr
  = CharLiteral Char
  | StringLiteral Text
  | NumberLiteral Double
  | BoolLiteral Bool
  | VariableExpr Identifier
  | FunctionExpr Identifier [Expr]
  | UnaryOp Oprt Expr
  | BinaryOp Oprt Expr Expr
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

genHeader :: Program -> String
genHeader (Program p) = intercalate "\n" $ map showDecl p
  where
    showDecl (FunctionDecl t i args _) =
      printf "%s %s(%s);" (show t) i (showArgs args)
    showDecl (VariableDecl t i _) =
      printf "%s %s;" (show t) i

    showArgs args =
      intercalate ", " $ map (\(ty, ident) -> printf "%s %s" (show ty) ident) args
