module Main where

import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Parser (pProgram)
import Semant (analyseProgram)
import System.Environment (getArgs)
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file : _) -> do
      input <- T.readFile file
      let result = runParser pProgram file input
      case result of
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> do
          case analyseProgram [Map.empty] res of
            Left err -> print err
            Right program -> print program
    _ -> do
      putStrLn "No input files."
