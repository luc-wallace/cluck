module Parser where

import Lexer

type Identifier = String

data AST
  = Program [Declaration]

data Statement
  = Return Expression
  deriving (Show)

data Expression
  = Literal Literal
  deriving (Show)

data Declaration
  = Variable Identifier Expression
  | Function Type Identifier [Statement]
  deriving (Show)

data Type
  = Invalid
  | Int
  deriving (Show)

parseBlock :: Token -> Token -> [Token] -> ([Token], [Token])
parseBlock start end ts = let (block, rest) = parseBlock' start end ts $ -1 in (block, rest)

parseBlock' :: Token -> Token -> [Token] -> Int -> ([Token], [Token])
parseBlock' _ _ [] _ = ([], [])
parseBlock' start end (t : ts) depth
  | t == start = go $ depth + 1
  | t == end = if depth == 0 then ([t], ts) else go $ depth - 1
  | otherwise = go depth
  where
    go d =
      let (inner, rest) = parseBlock' start end ts d
       in (t : inner, rest)

-- parse :: [Token] -> Program
-- parse t = let parseDeclaration

parseDeclaration :: [Token] -> (Maybe Declaration, [Token])
parseDeclaration ts =
  let (block, rest) = parseBlock (Symbol OpenBrace) (Symbol CloseBrace) ts
   in case block of
        [Keyword kw, Symbol OpenBrace, Identifier name, _, Symbol CloseBrace] ->
          ( Just
              ( Function
                  ( case kw of
                      Lexer.Int -> Parser.Int
                      _ -> Invalid
                  )
                  name
                  []
              ),
            rest
          )
        _ -> (Nothing, block ++ rest)

parseStatement :: [Token] -> [Statement]
parseStatement _ = undefined

parseExpression :: [Token] -> Expression
parseExpression _ = undefined
