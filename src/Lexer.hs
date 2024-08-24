module Lexer where

import Data.Char

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

-- base recursive lexing function
lexStr :: String -> [Token]
lexStr (ch : chs)
  | ch == '{' = Symbol OpenBrace : rest
  | ch == '}' = Symbol CloseBrace : rest
  | ch == '(' = Symbol OpenParen : rest
  | ch == ')' = Symbol CloseParen : rest
  | ch == ';' = Symbol Semi : rest
  | isSpace ch = rest
  | isAlpha ch = let (id, rest) = lexIdentifier (ch : chs) in id : lexStr rest
  | isDigit ch = let (id, rest) = lexNumber (ch : chs) in id : lexStr rest
  | otherwise = error ("Invalid character: " ++ [ch])
  where
    rest = lexStr chs
lexStr "" = []

-- greedily consumes characters until an invalid character is encountered, returns the rest
lexIdentifier :: String -> (Token, String)
lexIdentifier s =
  let (id, rest) = span (\c -> isAlphaNum c || c == '_') s
   in case id of
        "return" -> (Keyword Return, rest)
        "int" -> (Keyword Int, rest)
        _ -> (Identifier id, rest)

-- greedily consumes characters until an invalid digit is encountered, returns the rest
lexNumber :: String -> (Token, String)
lexNumber s = (Literal $ Integer $ read id, rest) where (id, rest) = span isDigit s
