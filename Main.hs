module Main where

import Compiler
import Evaluator
import Grammar
import Parser
import System.Environment
import Text.Printf -- !! MUST REMEMBER TO REMOVE THIS !!

-- compile command in terminal:
-- ghc Main.hs -o Main
main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile] -> do
      sourceCode <- readFile sourceFile
      case parse parseProgram sourceCode of
        [(program, "")] -> do
          let compiledCode = programCode program
          stack <- execTAM [] compiledCode 0
          print stack
        _ -> putStrLn "Syntax error"
    _ -> putStrLn "Usage: ./Main <sourceFile>"

runFile :: FilePath -> IO ()
runFile sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      let compiledCode = programCode program
      stack <- execTAM [] compiledCode 0
      putStrLn $ "Final Stack: " ++ show stack
    _ -> putStrLn "Syntax error"

traceRunFile :: FilePath -> IO ()
traceRunFile sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      let compiledCode = programCode program
      stack <- traceTAM [] compiledCode
      putStrLn $ "Final Stack: " ++ show stack
    _ -> putStrLn "Syntax error"

getInsts :: FilePath -> IO [TAMInst]
getInsts sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> return $ programCode program
    _ -> error "Syntax error"
