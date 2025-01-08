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
import System.Directory (removeFile)
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
  Options inp out mode <- execParser options

  (inFile, source) <- case inp of
    FileInput f -> do
      content <- T.readFile f
      return (f, content)
    StdInput -> do
      content <- IO.getContents
      return ("<stdin>", content)

  case runParser pProgram inFile source of
    Left err -> putStrLn (errorBundlePretty err) *> exitFailure
    Right ast ->
      if mode == AST
        then print ast
        else case analyseProgram ast of
          Left err -> print err *> exitFailure
          Right sast -> do
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
                    writeFile "temp.ll" llvm
                    -- putStr =<< readProcess "clang-14" (["-w", "-x", "ir", "-", "-o"] ++ [output]) llvm
                    putStr =<< readProcess "clang-14" (["-w", "-lm", "temp.ll", "-O2", "-o"] ++ [output]) llvm
                    removeFile "temp.ll"
                  _ -> error "error: unreachable"
