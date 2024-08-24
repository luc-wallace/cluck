module Main where

import Lexer
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      input <- readFile file
      let tokens = lexStr input
      print tokens
    _ -> putStrLn "No input file passed"
