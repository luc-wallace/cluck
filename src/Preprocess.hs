module Preprocess where

import Ast
import Data.List (intercalate)
import Text.Printf (printf)

genHeader :: Program -> String
genHeader (Program p) = intercalate "\n\n" (map showDecl p) ++ "\n"
  where
    showDecl (FunctionDecl t i args _) =
      printf "%s %s(%s);" (show t) i (showArgs args)
    showDecl (VariableDecl t i _) =
      printf "%s %s;" (show t) i

    showArgs args =
      intercalate ", " $ map (\(ty, ident) -> printf "%s %s" (show ty) ident) args
