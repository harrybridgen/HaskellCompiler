module Main where

import Compiler
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Evaluator
import Grammar
import Parser
import System.Environment
import TAM

-- Compile in terminal:
-- ghc Main.hs -o mtc
-- ./mtc factorial.mt will compile the MiniTriangle (.mt) program to TAM code (.tam)
-- ./mtc factorial.tam will execute the compiled TAM code
main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile] ->
      if ".mt" `isSuffixOf` sourceFile
        then do
          sourceCode <- readFile sourceFile
          case parse parseProgram sourceCode of
            [(program, "")] -> do
              let compiledCode = compileProgram program
              let tamFile = newFile sourceFile
              writeFile tamFile (unlines $ map show compiledCode)
              putStrLn $ "Compiled " ++ sourceFile ++ " to " ++ tamFile
            _ -> putStrLn "Syntax error"
        else
          if ".tam" `isSuffixOf` sourceFile
            then do
              tamCode <- map read . lines <$> readFile sourceFile
              putStrLn "Executing TAM code"
              stack <- execTAM [] tamCode 0
              putStrLn $ "Final stack: " ++ show stack
            else
              putStrLn "Use .mt for source files or .tam for compiled files"
    _ -> putStrLn "Usage: ./Main example.mt"

newFile :: FilePath -> FilePath
newFile file = takeWhile (/= '.') file ++ ".tam"

runFile :: FilePath -> IO ()
runFile sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      let compiledCode = compileProgram program
      stack <- execTAM [] compiledCode 0
      putStrLn $ "Final Stack: " ++ show stack
    _ -> putStrLn "Syntax error"

traceRunFile :: FilePath -> IO ()
traceRunFile sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      let compiledCode = compileProgram program
      stack <- traceTAM [] compiledCode
      putStrLn $ "Final Stack: " ++ show stack
    _ -> putStrLn "Syntax error"

getInsts :: FilePath -> IO ()
getInsts sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      let compiledCode = compileProgram program
      mapM_ print compiledCode
    _ -> putStrLn "Syntax error"

stringTraceTAM :: String -> IO Stack
stringTraceTAM expression = do
  let instructions = compArith expression
  traceTAM [] instructions

compArith :: String -> [TAMInst]
compArith expression = case parse parseExpr expression of
  [(ast, "")] -> expCode ast []
  _ -> error "Compilation error"