module Main where

import Compiler
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Grammar
import Parser
import State
import System.Environment
import TAM
import TypeChecker

-- Compile in terminal:
-- ghc Main.hs -o mtc
-- ./mtc factorial.mt will compile the MiniTriangle (.mt) program to TAM code (.tam)
-- ./mtc factorial.tam will execute the compiled TAM code
main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile] -> processFile sourceFile
    _ -> putStrLn "Usage: ./Main example.mt"

processFile :: FilePath -> IO ()
processFile sourceFile
  | ".mt" `isSuffixOf` sourceFile = processSourceFile sourceFile
  | ".tam" `isSuffixOf` sourceFile = processTamFile sourceFile
  | otherwise = putStrLn "Use .mt for source files or .tam for compiled files"

processSourceFile :: FilePath -> IO ()
processSourceFile sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] ->
      case compileProgram program of
        Left err -> putStr err
        Right compiledCode -> putStrLn ("No type errors found for " ++ sourceFile) >> writeTamFile sourceFile compiledCode
    _ -> putStrLn "Syntax error"

processTamFile :: FilePath -> IO ()
processTamFile sourceFile = do
  tamCode <- parse parseTAMProgram <$> readFile sourceFile
  case tamCode of
    [(insts, "")] -> do
      putStrLn "Executing TAM code"
      stack <- execTAM insts
      putStrLn $ "Final stack: " ++ show stack
    _ -> putStrLn "Syntax error"

writeTamFile :: FilePath -> [TAMInst] -> IO ()
writeTamFile sourceFile compiledCode = do
  let tamFile = takeWhile (/= '.') sourceFile ++ ".tam"
  writeFile tamFile (foldMap (\inst -> showInst inst ++ "\n") compiledCode)
  putStrLn $ "Compiled " ++ sourceFile ++ " to " ++ tamFile

-- Functions for testing in GHCi
runMT :: FilePath -> IO ()
runMT sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      case compileProgram program of
        Left err -> putStr err
        Right compiledCode -> do
          putStrLn ("No type errors found for " ++ sourceFile)
          stack <- execTAM compiledCode
          putStrLn $ "Final Stack: " ++ show stack
    _ -> putStrLn "Syntax error"

runTraceMT :: FilePath -> IO ()
runTraceMT sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      putStrLn "Executing TAM code"
      case compileProgram program of
        Left err -> putStr err
        Right compiledCode -> do
          putStrLn ("No type errors found for " ++ sourceFile)
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
    _ -> putStrLn "Syntax error"

runTraceTAM :: FilePath -> IO ()
runTraceTAM sourceFile = do
  tamCode <- parse parseTAMProgram <$> readFile sourceFile
  case tamCode of
    [(tamCode, "")] -> do
      putStrLn "Executing TAM code"
      stack <- traceTAM tamCode
      putStrLn $ "Final stack: " ++ show stack
    _ -> putStrLn "Syntax error"

instsMT :: FilePath -> IO ()
instsMT sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      case compileProgram program of
        Left err -> putStrLn err
        Right compiledCode -> do
          putStrLn ("No type errors found for " ++ sourceFile)
          putStrLn "TAM instructions:"
          forM_ compiledCode (putStrLn . showInst)
    _ -> putStrLn "Syntax error"

parseSource :: FilePath -> IO Program
parseSource sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> return program
    _ -> error "Syntax error"

parseSourceCheck :: FilePath -> IO ()
parseSourceCheck sourceFile = do
  sourceCode <- readFile sourceFile
  case parse parseProgram sourceCode of
    [(program, "")] -> do
      let (errors, _) = runState (checkProgram program) []
      if null errors
        then putStrLn "No type errors found"
        else putStrLn $ unlines errors
    _ -> putStrLn "Syntax error"
