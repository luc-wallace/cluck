module Lexer where

import Data.Char
import Token

-- base lexing function
tokenize :: String -> [Token]
tokenize (ch : chs)
  | ch == '{' = Symbol OpenBrace : rest
  | ch == '}' = Symbol CloseBrace : rest
  | ch == '(' = Symbol OpenParen : rest
  | ch == ')' = Symbol CloseParen : rest
  | ch == ';' = Symbol Semi : rest
  | isSpace ch = rest
  | isAlpha ch = let (id, rest) = lexIdentifier (ch : chs) in id : tokenize rest
  | isDigit ch = let (id, rest) = lexNumber (ch : chs) in id : tokenize rest
  | otherwise = error ("Invalid character: " ++ [ch])
  where
    rest = tokenize chs
tokenize [] = []

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
