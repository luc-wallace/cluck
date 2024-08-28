module Main where

import Lexer
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      input <- readFile file
      let tokens = tokenize input
      let (block, rest) = parseBlock (Symbol OpenBrace) (Symbol CloseBrace) tokens
      print block
      print rest
    _ -> putStrLn "No input file passed"
