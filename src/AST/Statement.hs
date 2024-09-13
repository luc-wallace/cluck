module AST.Stmt where

data Stmt = Block Block | Expression Expr

data Block = Body [Block]

