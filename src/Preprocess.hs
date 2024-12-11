module Preprocess where

import Ast
import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Text.Printf (printf)

data Line
  = Include Text
  | Define Text
  | Source Text

data Env = Env

data PreprocessError
  = SyntaxError

type Preprocess = ExceptT PreprocessError (State Env)

genHeader :: Program -> String
genHeader (Program p) = intercalate "\n\n" (map showDecl p) ++ "\n"
  where
    showDecl (FunctionDecl t i args _) =
      printf "%s %s(%s);" (show t) i (showArgs args)
    showDecl (VariableDecl t i _) =
      printf "%s %s;" (show t) i

    showArgs args =
      intercalate ", " $ map (\(ty, ident) -> printf "%s %s" (show ty) ident) args

-- preprocess :: Text -> Preprocess Text
-- preprocess source = intercalate "\n" $ mapM (processLine . unpack) $ lines source

-- processLine :: String -> Preprocess Line
-- processLine l@(x : _) = case x of
--   '#' -> pDirective l
--   _ -> pure $ Source $ pack l
