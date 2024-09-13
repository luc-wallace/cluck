module AST.Expr where

data Expr
  = Literal Literal
  | UnaryExpr
  | BinaryExpr

data Literal
  = Int Int
  | String String
  | Symbol Token.Symbol

data BinaryExpr
  = Left Expr
  | Op Token.Token
  | Right Expr

data UnaryExpr
  = Op Token.Token
  | Right Expr
