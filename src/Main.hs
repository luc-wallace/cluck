module Main where

import Codegen (codegenProgram)
import qualified Data.Text.IO as IO
import qualified Data.Text.IO as T
import Data.Text.Lazy
import LLVM.Pretty (ppllvm)
import Options.Applicative
import Parser (pProgram)
import Semant (analyseProgram)
import System.Exit
import Text.Megaparsec

data Input
  = FileInput FilePath
  | StdInput

data Output
  = FileOutput FilePath
  | StdOutput

data Options = Options Input Output

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
    <|> pure StdOutput

options :: ParserInfo Options
options =
  info
    (Options <$> pInput <*> pOutput)
    ( fullDesc
        <> header "cluck - a tiny c compiler"
    )

main :: IO ()
main = do
  Options inp out <- execParser options

  (inFile, source) <- case inp of
    FileInput f -> do
      content <- T.readFile f
      return (f, content)
    StdInput -> do
      content <- IO.getContents
      return ("<stdin>", content)

  case runParser pProgram inFile source of
    Left err -> putStrLn (errorBundlePretty err) *> exitFailure
    Right ast -> case analyseProgram ast of
      Left err -> print err *> exitFailure
      Right sast ->
        let llvm = unpack . ppllvm . codegenProgram $ sast
         in case out of
              FileOutput outFile -> writeFile outFile llvm
              StdOutput -> putStrLn llvm
