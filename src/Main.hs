module Main where

import Parser (pProgram)
import Text.Megaparsec
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (file : _) -> do
      input <- readFile file
      let result = runParser pProgram file input
      case result of
        Left err -> putStrLn (errorBundlePretty err)
        Right res -> print res
    _ -> do
      putStrLn "No input files."
