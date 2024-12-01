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

pWord :: Text -> Parser ()
pWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

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

pIdent :: Parser Text
pIdent = label "identifier" $ do
  ident <- (:) <$> letterChar <*> many nonDigit
  when (pack ident `elem` keywords) $ fail "identifier cannot be keyword"
  return $ pack ident

pBaseType :: Parser Type
pBaseType =
  choice
    [ Int <$ pWord "int",
      Float <$ pWord "float",
      Bool <$ pWord "bool",
      Char <$ pWord "char",
      Void <$ pWord "void"
    ]

pType :: Parser Type
pType = do
  base <- pBaseType
  pointers <- many $ try $ space *> string "*"
  return $ foldr (const Pointer) base pointers

pVarDecl :: Parser Decl
pVarDecl =
  VariableDecl <$> lexeme pType
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

pDecl :: Parser Decl
pDecl = try pVarDecl <|> pFuncDecl

pFuncDecl :: Parser Decl
pFuncDecl =
  FunctionDecl
    <$> lexeme pType
    <*> lexeme pIdent
    <*> between (symbol "(") (symbol ")") (pFuncArg `sepBy` symbol ",")
    <*> (Just <$> lexeme pBlockStmt <|> symbol ";" *> pure Nothing)

pFuncArg :: Parser Arg
pFuncArg = (,) <$> lexeme pType <*> lexeme pIdent

pExpr :: Parser Expr
pExpr = label "expression" (makeExprParser pTerm operatorTable)

pNum :: Parser Expr
pNum = IntLiteral <$> L.decimal

pFloat :: Parser Expr
pFloat = FloatLiteral <$> L.float

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
        pDoWhileStmt,
        pVarDeclStmt,
        pExprStmt
      ]

pVarDeclStmt :: Parser Stmt
pVarDeclStmt =
  VariableDeclStmt
    <$> lexeme pType
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

pReturnStmt :: Parser Stmt
pReturnStmt = ReturnStmt <$ pWord "return" <*> optional (lexeme pExpr) <* symbol ";"

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr <* symbol ";"

pBlockStmt :: Parser Stmt
pBlockStmt = BlockStmt <$> between (symbol "{") (symbol "}") (many pStmt)

pIfStmt :: Parser Stmt
pIfStmt = IfStmt <$ pWord "if" <*> lexeme pParens <*> lexeme pStmt <*> optional (pWord "else" *> pStmt)

pDoWhileStmt :: Parser Stmt
pDoWhileStmt = DoWhileStmt <$ pWord "do" <*> lexeme pStmt <* pWord "while" <*> lexeme pParens <* symbol ";"

pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

pCast :: Parser Expr
pCast = Cast <$> lexeme (between (symbol "(") (symbol ")") pType) <*> pExpr

pTerm :: Parser Expr
pTerm =
  lexeme $
    choice
      [ try pCast,
        try pFloat,
        try pBoolLiteral,
        pNum,
        try pFuncExpr,
        try pVarExpr,
        pParens,
        pStringLiteral,
        pCharLiteral
      ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "*" $ UnaryOp Deref,
      prefix "&" $ UnaryOp Ref,
      prefix "!" $ UnaryOp Not,
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
      binary ">=" $ BinaryOp GtOrEqTo,
      binary ">" $ BinaryOp Gt,
      binary "<=" $ BinaryOp LtOrEqTo,
      binary "<" $ BinaryOp Lt
    ],
    [ binary "&&" $ BinaryOp And,
      binary "||" $ BinaryOp Or
    ],
    [ binary "=" $ BinaryOp Assign
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

pProgram :: Parser Program
pProgram = Program <$ lexeme space <*> many (lexeme pDecl) <* eof
