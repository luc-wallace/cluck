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
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pWord :: Text -> Parser ()
pWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

integer :: Parser Integer
integer = lexeme L.decimal

nonDigit :: Parser Char
nonDigit = alphaNumChar <|> char '_'

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

keywords :: [Text]
keywords =
  [ "break",
    "case",
    "char",
    "if",
    "continue",
    "do",
    "sizeof",
    "switch",
    "double",
    "else",
    "float",
    "for",
    "int",
    "long",
    "return",
    "short",
    "void",
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

pProgram :: Parser Program
pProgram = Program <$ lexeme space <*> many (lexeme pDecl) <* eof

pDecl :: Parser Decl
pDecl = try pVarDecl <|> pFuncDecl

pVarDecl :: Parser Decl
pVarDecl =
  VariableDecl <$> lexeme pType
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

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

pTerm :: Parser Expr
pTerm =
  lexeme $
    choice
      [ try $ Cast <$> lexeme (pParens pType) <*> pExpr,
        try $ FloatLiteral <$> L.float,
        try $ BoolLiteral <$> (True <$ string "true" <|> False <$ string "false"),
        try $ SizeOf <$ pWord "sizeof" <*> pParens pType,
        try $ Null <$ pWord "NULL",
        IntLiteral <$> L.decimal,
        CharLiteral <$> between (char '\'') (char '\'') L.charLiteral,
        try $ FunctionExpr <$> lexeme pIdent <*> between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ","),
        try $ ArrayExpr <$> lexeme pIdent <* symbol "[" <*> L.decimal <* symbol "]",
        try $ VariableExpr <$> pIdent,
        pParens pExpr
      ]

pStmt :: Parser Stmt
pStmt =
  lexeme $
    choice
      [ pBlockStmt,
        pReturnStmt,
        pBreakStmt,
        pContinueStmt,
        pIfStmt,
        pDoWhileStmt,
        pForStmt,
        pWhileStmt,
        try pVarDeclStmt,
        pArrayDeclStmt,
        pExprStmt
      ]

pVarDeclStmt :: Parser Stmt
pVarDeclStmt =
  VariableDeclStmt
    <$> lexeme pType
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

pArrayDeclStmt :: Parser Stmt
pArrayDeclStmt =
  ArrayDeclStmt
    <$> lexeme pType
    <*> lexeme pIdent
    <* symbol "["
    <*> optional L.decimal
    <* symbol "]"
    <*> optional (symbol "=" *> between (symbol "{") (symbol "}") (pExpr `sepBy` symbol ","))
    <* symbol ";"

pBreakStmt :: Parser Stmt
pBreakStmt = BreakStmt <$ pWord "break" <* symbol ";"

pContinueStmt :: Parser Stmt
pContinueStmt = ContinueStmt <$ pWord "continue" <* symbol ";"

pReturnStmt :: Parser Stmt
pReturnStmt = ReturnStmt <$ pWord "return" <*> optional (lexeme pExpr) <* symbol ";"

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr <* symbol ";"

pBlockStmt :: Parser Stmt
pBlockStmt = BlockStmt <$> between (symbol "{") (symbol "}") (many pStmt)

pIfStmt :: Parser Stmt
pIfStmt = IfStmt <$ pWord "if" <*> lexeme (pParens pExpr) <*> lexeme pStmt <*> optional (pWord "else" *> pStmt)

pDoWhileStmt :: Parser Stmt
pDoWhileStmt = DoWhileStmt <$ pWord "do" <*> lexeme pStmt <* pWord "while" <*> lexeme (pParens pExpr) <* symbol ";"

pForStmt :: Parser Stmt
pForStmt = do
  pWord "for"
  (e1, e2, e3) <- between (symbol "(") (symbol ")") $ do
    e1 <- lexeme pExpr
    _ <- symbol ";"
    e2 <- lexeme pExpr
    _ <- symbol ";"
    e3 <- lexeme pExpr
    return (e1, e2, e3)
  ForStmt e1 e2 e3 <$> lexeme pStmt

pWhileStmt :: Parser Stmt
pWhileStmt = WhileStmt <$ pWord "while" <*> lexeme (pParens pExpr) <*> pStmt

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "*" $ UnaryOp Deref,
      prefix "&" $ UnaryOp Ref,
      prefix "!" $ UnaryOp Not,
      prefix "-" $ UnaryOp Neg,
      prefix "+" id,
      postfix "++" $ UnaryOp Inc,
      postfix "--" $ UnaryOp Dec
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
