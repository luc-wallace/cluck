module Parser where

import Ast
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

nonDigit :: Parser Char
nonDigit = alphaNumChar <|> char '_'

pOptionalInt :: Parser (Maybe ())
pOptionalInt = optional (try (space1 <* string "int") <|> pure ())

pType :: Parser Type
pType =
  choice
    [ Void <$ string "void",
      try (LongDouble <$ string "long" <* space1 <* string "double"),
      Double <$ string "double",
      Float <$ string "float",
      Char <$ string "char",
      try (UnsignedInt <$ string "unsigned" <* space1 <* string "int"),
      try (UnsignedChar <$ string "unsigned" <* space1 <* string "char"),
      SignedChar <$ string "signed" <* space1 <* string "char",
      Int <$ string "int",
      Long <$ string "long" <* pOptionalInt,
      Short <$ string "short" <* pOptionalInt,
      try (UnsignedShort <$ string "unsigned" <* space1 <* string "short" <* pOptionalInt),
      UnsignedLong <$ string "unsigned" <* space1 <* string "long" <* pOptionalInt
    ]

pIdent :: Parser String
pIdent = label "identifier" $ do
  x <- letterChar
  xs <- many nonDigit
  return (x : xs)

pVarDecl :: Parser Decl
pVarDecl =
  VariableDecl
    <$> pType
    <* space1
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

pDecl :: Parser Decl
pDecl = do try pVarDecl <|> pFuncDecl

pFuncDecl :: Parser Decl
pFuncDecl =
  FunctionDecl
    <$> pType
    <* space1
    <*> lexeme pIdent
    <* symbol "("
    <* symbol ")"
    <*> (Just <$> lexeme pBlockStmt <|> (symbol ";" *> pure Nothing))

pExpr :: Parser Expr
pExpr = label "expression" (makeExprParser pTerm operatorTable)

pNum :: Parser Expr
pNum = Number <$> try (L.decimal <|> L.float)

pVarExpr :: Parser Expr
pVarExpr = VariableExpr <$> pIdent

pStmt :: Parser Stmt
pStmt =
  lexeme $
    choice
      [ pBlockStmt,
        pReturnStmt,
        pIfStmt,
        pVarDeclStmt,
        pVarAssignStmt,
        pExprStmt
      ]

pVarDeclStmt :: Parser Stmt
pVarDeclStmt = VariableDeclStmt <$> pVarDecl

pVarAssignStmt :: Parser Stmt
pVarAssignStmt = VariableAssignStmt <$> lexeme pIdent <* symbol "=" <*> lexeme pExpr <* symbol ";"

pReturnStmt :: Parser Stmt
pReturnStmt = ReturnStmt <$ string "return" <* space1 <*> lexeme pExpr <* symbol ";"

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr <* symbol ";"

pBlockStmt :: Parser Stmt
pBlockStmt = BlockStmt <$> between (symbol "{") (symbol "}") (many pStmt)

pIfStmt :: Parser Stmt
pIfStmt = IfStmt <$ symbol "if" <*> pParens <*> pStmt

pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

pTerm :: Parser Expr
pTerm =
  lexeme $
    choice
      [ pNum,
        pVarExpr,
        pParens
      ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "!" Not,
      prefix "-" Neg,
      prefix "+" id
    ],
    [ binary "*" Mul,
      binary "/" Div,
      binary "%" Mod
    ],
    [ binary "+" Add,
      binary "-" Sub
    ],
    [ binary "==" EqTo,
      binary "!=" NtEqTo,
      binary ">" Gt,
      binary ">=" GtOrEqTo,
      binary "<" Lt,
      binary "<=" LtOrEqTo
    ],
    [ binary "&&" And,
      binary "||" Or
    ]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

pProgram :: Parser [Decl]
pProgram = many pDecl <* eof
