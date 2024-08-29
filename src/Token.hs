module Token where

-- root token type
data Token
  = Keyword Keyword
  | Literal Literal
  | Symbol Symbol
  | Identifier String
  deriving (Show, Eq)

data Keyword
  = Return
  | Int
  deriving (Show, Eq)

data Symbol
  = OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Semi
  deriving (Show, Eq)

data Literal
  = String String
  | Integer Int
  deriving (Show, Eq)
