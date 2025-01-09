module Main where

import Codegen (codegenProgram)
import qualified Data.Text.IO as IO
import qualified Data.Text.IO as T
import Data.Text.Lazy
import LLVM.Pretty (ppllvm)
import Options.Applicative
import Parser (pProgram)
import Preprocess (genHeader)
import Semant (analyseProgram)
import System.Directory (findExecutable, removeFile)
import System.Exit
import System.FilePath
import System.Process
import Text.Megaparsec

-- TODO: check if clang is installed

data Input
  = FileInput FilePath
  | StdInput

data Output
  = FileOutput FilePath
  | DefaultOutput

data Mode
  = AST
  | Header
  | LLVM
  | Binary
  deriving (Eq)

data Options = Options Input Output Mode

pInput :: Parser Input
pInput = FileInput <$> argument str (metavar "FILE") <|> pure StdInput

pOutput :: Parser Output
pOutput =
  FileOutput
    <$> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
      )
    <|> pure DefaultOutput

pMode :: Parser Mode
pMode =
  flag Binary AST (long "ast" <> short 'A')
    <|> flag Binary LLVM (long "assemble" <> short 'S')
    <|> flag Binary Header (long "header" <> short 'H')

options :: ParserInfo Options
options =
  info
    (Options <$> pInput <*> pOutput <*> pMode)
    ( fullDesc
        <> header "cluck - a tiny c compiler"
    )

main :: IO ()
main = do
  -- get user command line options
  Options inp out mode <- execParser options

  (inFile, source) <- case inp of
    FileInput f -> do
      content <- T.readFile f
      return (f, content)
    StdInput -> do
      content <- IO.getContents
      return ("<stdin>", content)

  -- run parser on source file
  case runParser pProgram inFile source of
    -- Left: parser failed
    Left err -> putStrLn (errorBundlePretty err) *> exitFailure
    -- Right: parser succeeded
    Right ast ->
      if mode == AST
        then print ast
        else -- run semantic analysis on AST
        case analyseProgram ast of
          -- Left: semant failed
          Left err -> print err *> exitFailure
          -- Right: semant succeeded
          Right sast -> do
            -- get output file name and extension
            let output = case out of
                  FileOutput outFile -> outFile
                  DefaultOutput -> do
                    case inFile of
                      "<stdin>" -> "out"
                      _ ->
                        takeBaseName inFile ++ case mode of
                          Header -> ".h"
                          LLVM -> ".ll"
                          _ -> ""
            case mode of
              Header -> writeFile output $ genHeader ast
              _ -> do
                let llvm = unpack . ppllvm . codegenProgram $ sast

                case mode of
                  LLVM -> writeFile output llvm
                  Binary -> do
                    -- check if clang-14 or clang is installed, else error
                    clangExec <- findExecutable "clang-14"
                    clang <- case clangExec of
                      Just _ -> return "clang-14"
                      Nothing -> do
                        fallback <- findExecutable "clang"
                        case fallback of
                          Just _ -> return "clang"
                          Nothing -> putStrLn "error: neither 'clang-14' nor 'clang' are installed on your system" *> exitFailure

                    -- write LLVM IR to temporary .ll file and delete it after clang has done its job
                    writeFile "temp.ll" llvm

                    -- use clang to compile .ll file with -O2 optimisation
                    putStr =<< readProcess clang (["-w", "-lm", "temp.ll", "-O2", "-o"] ++ [output]) llvm

                    removeFile "temp.ll"
                  _ -> error "error: unreachable"
