module Analysis where

import Ast

-- Seq is True if statement is a return statement
data CFG = Empty | Seq Bool CFG | Branch CFG CFG deriving (Show)

-- checks if all paths have a return statement
validate :: CFG -> Bool
validate Empty = False
validate (Seq isRet cfg) = isRet || validate cfg
validate (Branch l r) = validate l && validate r

-- creates control flow graph of statement execution
genCFG :: [Stmt] -> CFG
genCFG [] = Empty
genCFG (s : ss) = case s of
  ReturnStmt _ -> Seq True $ genCFG ss
  BlockStmt stmts -> genCFG (stmts ++ ss)
  IfStmt _ t e ->
    let base = genCFG $ t : ss
     in case e of
          Nothing -> Branch base (genCFG ss)
          Just stmt -> Branch base (genCFG $ stmt : ss)
  _ -> Seq False $ genCFG ss
