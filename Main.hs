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

printTAMInstructions :: FilePath -> IO ()
printTAMInstructions sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      let compiledCode = programCode program
      putStrLn "TAM Instructions:"
      mapM_ print compiledCode
    _ -> putStrLn "Syntax error"
