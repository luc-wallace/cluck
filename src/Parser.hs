module Parser where

import qualified AST as A
import qualified Token as T

parseBlock :: T.Token -> T.Token -> [T.Token] -> ([T.Token], [T.Token])
parseBlock start end ts = let (block, rest) = parseBlock' start end ts $ -1 in (block, rest)

parseBlock' :: T.Token -> T.Token -> [T.Token] -> Int -> ([T.Token], [T.Token])
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

keywordToType :: T.Keyword -> A.Type
keywordToType kw = case kw of
  T.Int -> A.Integer
  _ -> A.Invalid

parseDeclaration :: [T.Token] -> (Maybe A.Declaration, [T.Token])
parseDeclaration ts =
  let (block, rest) = parseBlock (T.Symbol T.OpenBrace) (T.Symbol T.CloseBrace) ts
   in case block of
        (T.Keyword kw : T.Identifier name : T.Symbol T.OpenParen : T.Symbol T.CloseParen : T.Symbol T.OpenBrace : rs) ->
          ( case reverse rs of
              (T.Symbol T.CloseBrace : xs) ->
                ( Just
                    (A.Function (keywordToType kw) name (parseConstructs (reverse xs))),
                  rest
                )
              _ -> error "Expected '}'"
          )
        _ -> (Nothing, block ++ rest)

parseConstructs :: [T.Token] -> [A.Construct]
parseConstructs ts = case parseConstructs' ts of
  (cs, []) -> cs
  (_, rest) -> error ("remaining tokens: " ++ show rest)

parseConstructs' :: [T.Token] -> ([A.Construct], [T.Token])
parseConstructs' [] = ([], [])
parseConstructs' ts = case ts of
  (T.Keyword kw : T.Identifier name : T.Symbol T.Semi : rs) ->
    (A.Declaration (A.Variable (keywordToType kw) name Nothing) : constructs, rest)
    where
      (constructs, rest) = parseConstructs' rs
  (T.Keyword T.Return : rs) -> (A.Statement (A.Return (A.Literal $ T.Integer 0)) : constructs, rest)
    where
      (constructs, rest) = parseConstructs' rs
  _ -> error ("parse error: " ++ show ts)

-- parseConstruct (T.Keyword kw : T.Identifier name : T.Symbol T.Semi : ts) = ([A.Declaration $ A.Variable (A.Integer) name Nothing], ts)
-- parseConstruct (T.Keyword T.Return : ts) = ([A.Statement $ A.Return (A.Literal (T.Integer 0))], ts)
-- parseConstruct _ = ([], [])

parseExpression :: [T.Token] -> A.Expression
parseExpression _ = undefined
