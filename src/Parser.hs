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

-- consumes whitespace and comments - functions as a lexer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- run any parser and consume all whitespace after
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parse any string and consume all whitespace after
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- parse any string of characters, must be terminated by another character
pWord :: Text -> Parser ()
pWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- parse any sequence of digits
integer :: Parser Integer
integer = lexeme L.decimal

-- letter, digit or underscore
nonDigit :: Parser Char
nonDigit = alphaNumChar <|> char '_'

-- runs a parser between parentheses
pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

-- list of all cluck keywords
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
    "return",
    "void",
    "while"
  ]

-- checks that identifiers do not start with a number and are not a reserved keyword
pIdent :: Parser Text
pIdent = label "identifier" $ do
  ident <- (:) <$> letterChar <*> many nonDigit
  when (pack ident `elem` keywords) $ fail "identifier cannot be keyword"
  return $ pack ident

-- parse basic primitive types
pBaseType :: Parser Type
pBaseType =
  choice
    [ Int <$ pWord "int",
      Float <$ pWord "float",
      Bool <$ pWord "bool",
      Char <$ pWord "char",
      Void <$ pWord "void"
    ]

-- parse all primitive types including pointers
pType :: Parser Type
pType = label "type specifier" $ do
  base <- pBaseType
  pointers <- many $ try $ space *> string "*"
  return $ foldr (const Pointer) base pointers

-- top level parser - parse all declarations at top level
pProgram :: Parser Program
pProgram = Program <$ lexeme space <*> many (lexeme pDecl) <* eof

-- parse a variable or function declaration
pDecl :: Parser Decl
pDecl = try pVarDecl <|> pFuncDecl

pVarDecl :: Parser Decl
pVarDecl =
  VariableDecl <$> lexeme pType
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

-- parse a function declaration with an optional body (prototype function)
pFuncDecl :: Parser Decl
pFuncDecl =
  FunctionDecl
    <$> lexeme pType
    <*> lexeme pIdent
    <*> between (symbol "(") (symbol ")") ((try pArrayArg <|> pVarArg) `sepBy` symbol ",")
    <*> (Just <$> lexeme pBlockStmt <|> symbol ";" *> pure Nothing)

pVarArg :: Parser Arg
pVarArg = label "parameter" $ (,) <$> lexeme pType <*> lexeme pIdent

-- special case to handle array arguments to functions
pArrayArg :: Parser Arg
pArrayArg = do
  ty <- lexeme pType
  ident <- lexeme pIdent
  _ <- symbol "["
  _ <- (optional . lexeme) L.decimal
  _ <- symbol "]"
  pure (Pointer ty, ident)

-- combines term parser with operator table
pExpr :: Parser Expr
pExpr = label "expression" (makeExprParser pTerm operatorTable)

-- parses all basic expressions (terms) - try is used where grammars overlap
pTerm :: Parser Expr
pTerm =
  label "term expression" $
    lexeme $
      choice
        [ try $ Cast <$> lexeme (pParens pType) <*> pExpr,
          try $ FloatLiteral <$> L.float,
          try $ BoolLiteral <$> (True <$ string "true" <|> False <$ string "false"),
          try $ StringLiteral . pack <$ char '\"' <*> manyTill L.charLiteral (char '\"'),
          try $ SizeOfType <$ pWord "sizeof" <*> pParens pType,
          try $ SizeOfExpr <$ pWord "sizeof" <*> pParens pExpr,
          try $ Null <$ pWord "NULL",
          IntLiteral <$> L.decimal,
          CharLiteral <$> between (char '\'') (char '\'') L.charLiteral,
          try $ FunctionExpr <$> lexeme pIdent <*> between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ","),
          try $ ArrayExpr <$> lexeme pIdent <* symbol "[" <*> lexeme pExpr <* symbol "]",
          try $ VariableExpr <$> pIdent,
          pParens pExpr
        ]

-- parse any type of statement
pStmt :: Parser Stmt
pStmt =
  label "statement" $
    lexeme $
      choice
        [ pBlockStmt,
          pReturnStmt,
          pBreakStmt,
          pContinueStmt,
          pIfStmt,
          pSwitchStmt,
          pDoWhileStmt,
          pForStmt,
          pWhileStmt,
          try pVarDeclStmt,
          pArrayDeclStmt,
          pExprStmt
        ]

-- parse a variable declaration with an optional value
pVarDeclStmt :: Parser Stmt
pVarDeclStmt =
  VariableDeclStmt
    <$> lexeme pType
    <*> lexeme pIdent
    <*> optional (symbol "=" *> lexeme pExpr)
    <* symbol ";"

-- parse an array declaration with an optional list of initialisation values
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

-- parse if statement with optional else clause
pIfStmt :: Parser Stmt
pIfStmt = IfStmt <$ pWord "if" <*> lexeme (pParens pExpr) <*> pStmt <*> optional (pWord "else" *> pStmt)

pSwitchStmt :: Parser Stmt
pSwitchStmt = SwitchStmt <$ pWord "switch" <*> lexeme (pParens pExpr) <*> between (symbol "{") (symbol "}") (many pSwitchCase)

pSwitchCase :: Parser SwitchCase
pSwitchCase = SwitchCase <$ pWord "case" <*> lexeme pExpr <* symbol ":" <*> many pStmt

pDoWhileStmt :: Parser Stmt
pDoWhileStmt = DoWhileStmt <$ pWord "do" <*> pStmt <* pWord "while" <*> lexeme (pParens pExpr) <* symbol ";"

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
  ForStmt e1 e2 e3 <$> pStmt

pWhileStmt :: Parser Stmt
pWhileStmt = WhileStmt <$ pWord "while" <*> lexeme (pParens pExpr) <*> pStmt

-- operator precedence table, operators at the top have highest priority and binding power with expressions
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

-- parse infix expressions
binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (label "infix expression" $ f <$ symbol name)

-- parse prefix and posfix expressions
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (label "prefix expression" $ f <$ symbol name)
postfix name f = Postfix (label "postfix expression" $ f <$ symbol name)
