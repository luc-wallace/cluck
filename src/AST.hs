module AST where

import Token

type Identifier = String

newtype AST = Program [Declaration]

data Construct
  = Statement Statement
  | Expression Expression
  | Declaration Declaration
  deriving (Show)

data Statement
  = Return Expression
  deriving (Show)

data Expression
  = Literal Literal
  deriving (Show)

data Declaration
  = Variable Type Identifier (Maybe Expression)
  | Function Type Identifier [Construct]
  deriving (Show)

data Type
  = Invalid
  | Integer
  deriving (Show)
