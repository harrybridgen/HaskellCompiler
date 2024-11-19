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
              let tamFile = takeWhile (/= '.') sourceFile ++ ".tam"
              writeFile tamFile (foldMap (\inst -> showInst inst ++ "\n") compiledCode)
              putStrLn $ "Compiled " ++ sourceFile ++ " to " ++ tamFile
            _ -> putStrLn "Syntax error"
        else
          if ".tam" `isSuffixOf` sourceFile
            then do
              tamCode <- parse parseTAMProgram <$> readFile sourceFile
              case tamCode of
                [(insts, "")] -> do
                  putStrLn "Executing TAM code"
                  stack <- execTAM insts
                  putStrLn $ "Final stack: " ++ show stack
                _ -> putStrLn "Error: Invalid TAM syntax"
            else
              putStrLn "Use .mt for source files or .tam for compiled files"
    _ -> putStrLn "Usage: ./Main example.mt"

-- Functions for testing in GHCi
runMT :: FilePath -> IO ()
runMT sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      let compiledCode = compileProgram program
      stack <- execTAM compiledCode
      putStrLn $ "Final Stack: " ++ show stack
    _ -> putStrLn "Syntax error"

runTraceMT :: FilePath -> IO ()
runTraceMT sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      let compiledCode = compileProgram program
      stack <- traceTAM compiledCode
      putStrLn $ "Final Stack: " ++ show stack
    _ -> putStrLn "Syntax error"

runTAM :: FilePath -> IO ()
runTAM sourceFile = do
  tamCode <- parse parseTAMProgram <$> readFile sourceFile
  case tamCode of
    [(tamCode, "")] -> do
      putStrLn "Executing TAM code"
      stack <- execTAM tamCode
      putStrLn $ "Final stack: " ++ show stack

runTraceTAM :: FilePath -> IO ()
runTraceTAM sourceFile = do
  tamCode <- parse parseTAMProgram <$> readFile sourceFile
  case tamCode of
    [(tamCode, "")] -> do
      putStrLn "Executing TAM code"
      stack <- traceTAM tamCode
      putStrLn $ "Final stack: " ++ show stack

instsMT :: FilePath -> IO ()
instsMT sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      let compiledCode = compileProgram program
      putStrLn $ unlines $ map showInst compiledCode
    _ -> putStrLn "Syntax error"

traceExpr :: String -> IO Stack
traceExpr expression = do
  let instructions = compArith expression
  traceTAM instructions

compArith :: String -> [TAMInst]
compArith expression = case parse parseExpr expression of
  [(ast, "")] -> expCode ast []
  _ -> error "Compilation error"