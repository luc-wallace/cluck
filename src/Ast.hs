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
  deriving (Show, Eq)

typeStr :: Type -> Text
typeStr Char = "char"
typeStr SignedChar = "signed char"
typeStr UnsignedChar = "unsigned char"
typeStr Short = "short"
typeStr UnsignedShort = "unsigned short"
typeStr Int = "int"
typeStr UnsignedInt = "unsigned int"
typeStr Long = "long"
typeStr UnsignedLong = "unsigned long"
typeStr Float = "float"
typeStr Double = "double"
typeStr LongDouble = "long double"
typeStr Void = "void"
typeStr (Custom t) = t
typeStr (Pointer t) = append (typeStr t) "*"

data Decl
  = VariableDecl Type Identifier (Maybe Expr)
  | FunctionDecl Type Identifier [Arg] (Maybe Stmt)
  deriving (Show)

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
  deriving (Show, Eq)

genHeader :: Program -> String
genHeader (Program p) = intercalate "\n" $ map showDecl p
  where
    showDecl (FunctionDecl t i args _) =
      printf "%s %s(%s);" (typeStr t) i (showArgs args)
    showDecl (VariableDecl t i _) =
      printf "%s %s;" (typeStr t) i

    showArgs args =
      intercalate ", " $ map (\(ty, ident) -> printf "%s %s" (typeStr ty) ident) args
