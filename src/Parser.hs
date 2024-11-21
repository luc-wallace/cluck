module Parser where

import Ast
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

pCharLiteral :: Parser Expr
pCharLiteral = CharLiteral <$> between (char '\'') (char '\'') L.charLiteral

pStringLiteral :: Parser Expr
pStringLiteral = StringLiteral . pack <$ char '\"' <*> manyTill L.charLiteral (char '\"')

pBoolLiteral :: Parser Expr
pBoolLiteral = BoolLiteral <$> (True <$ string "true" <|> False <$ string "false")

nonDigit :: Parser Char
nonDigit = alphaNumChar <|> char '_'

pOptionalInt :: Parser (Maybe ())
pOptionalInt = optional (try (space1 <* string "int") <|> pure ())

keywords :: [Text]
keywords =
  [ "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "int",
    "long",
    "register",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while"
  ]

pType :: Parser Type
pType =
  choice
    [ try (Void <$ string "void"),
      try (LongDouble <$ string "long" <* space1 <* string "double"),
      try (Double <$ string "double"),
      try (Float <$ string "float"),
      try (Char <$ string "char"),
      try (UnsignedInt <$ string "unsigned" <* space1 <* string "int"),
      try (UnsignedChar <$ string "unsigned" <* space1 <* string "char"),
      try (SignedChar <$ string "signed" <* space1 <* string "char"),
      try (Int <$ string "int"),
      try (Long <$ string "long" <* pOptionalInt),
      try (Short <$ string "short" <* pOptionalInt),
      try (UnsignedShort <$ string "unsigned" <* space1 <* string "short" <* pOptionalInt),
      try (UnsignedLong <$ string "unsigned" <* space1 <* string "long" <* pOptionalInt),
      Custom <$> pIdent
    ]

pIdent :: Parser Text
pIdent = label "identifier" $ do
  ident <- (:) <$> letterChar <*> many nonDigit
  when (pack ident `elem` keywords) $ fail "identifier cannot be keyword"
  return $ pack ident

pVarDecl :: Parser Decl
pVarDecl =
  VariableDecl
    <$> pType
    <* space1
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

pDecl :: Parser Decl
pDecl = try pVarDecl <|> pFuncDecl

pFuncDecl :: Parser Decl
pFuncDecl =
  FunctionDecl
    <$> pType
    <* space1
    <*> lexeme pIdent
    <*> between (symbol "(") (symbol ")") (pFuncArg `sepBy` symbol ",")
    <*> (Just <$> lexeme pBlockStmt <|> symbol ";" *> pure Nothing)

pFuncArg :: Parser Arg
pFuncArg = (,) <$> lexeme pType <*> lexeme pIdent

pExpr :: Parser Expr
pExpr = label "expression" (makeExprParser pTerm operatorTable)

pNum :: Parser Expr
pNum = NumberLiteral <$> try (L.decimal <|> L.float)

pVarExpr :: Parser Expr
pVarExpr = VariableExpr <$> pIdent

pFuncExpr :: Parser Expr
pFuncExpr = FunctionExpr <$> lexeme pIdent <*> between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ",")

pStmt :: Parser Stmt
pStmt =
  lexeme $
    choice
      [ pBlockStmt,
        pReturnStmt,
        pIfStmt,
        try pVarDeclStmt,
        try pVarAssignStmt,
        pExprStmt
      ]

pVarDeclStmt :: Parser Stmt
pVarDeclStmt = VariableDeclStmt <$> pVarDecl

pVarAssignStmt :: Parser Stmt
pVarAssignStmt = VariableAssignStmt <$> lexeme pIdent <* symbol "=" <*> lexeme pExpr <* symbol ";"

pReturnStmt :: Parser Stmt
pReturnStmt = ReturnStmt <$ string "return" <*> optional (space1 *> lexeme pExpr) <* symbol ";"

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr <* symbol ";"

pBlockStmt :: Parser Stmt
pBlockStmt = BlockStmt <$> between (symbol "{") (symbol "}") (many pStmt)

pIfStmt :: Parser Stmt
pIfStmt = IfStmt <$ symbol "if" <*> pParens <*> pStmt <*> optional (symbol "else" *> pStmt)

pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

pTerm :: Parser Expr
pTerm =
  lexeme $
    choice
      [ pNum,
        try pFuncExpr,
        try pVarExpr,
        pParens,
        pBoolLiteral,
        pStringLiteral,
        pCharLiteral
      ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "!" $ UnaryOp Not,
      prefix "-" $ UnaryOp Neg,
      prefix "+" id
    ],
    [ binary "*" $ BinaryOp Mul,
      binary "/" $ BinaryOp Div,
      binary "%" $ BinaryOp Mod
    ],
    [ binary "+" $ BinaryOp Add,
      binary "-" $ BinaryOp Sub
    ],
    [ binary "==" $ BinaryOp EqTo,
      binary "!=" $ BinaryOp NtEqTo,
      binary ">" $ BinaryOp Gt,
      binary ">=" $ BinaryOp GtOrEqTo,
      binary "<" $ BinaryOp Lt,
      binary "<=" $ BinaryOp LtOrEqTo
    ],
    [ binary "&&" $ BinaryOp And,
      binary "||" $ BinaryOp Or
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

pProgram :: Parser Program
pProgram = Program <$ space <*> many pDecl <* eof
