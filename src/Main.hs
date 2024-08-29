module Main where

import Lexer (tokenize)
import Parser
import System.Environment (getArgs)
import Token

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      input <- readFile file
      let tokens = tokenize input
      let (block, rest) = parseBlock (Symbol OpenBrace) (Symbol CloseBrace) tokens
      print block
      putStrLn ""
      print $ parseDeclaration block
    _ -> putStrLn "No input file passed"
