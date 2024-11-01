module Main where

import System.Environment
import System.FilePath(takeExtension, replaceExtension)
import Grammar
import Parser
import Compiler
import Evaluator
import Text.Printf -- !! MUST REMEMBER TO REMOVE THIS !!

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, "--trace"] | takeExtension file == ".tam" -> do
      contents <- readFile file
      let tamInstructions = read contents :: [TAMInst]
      finalStack <- traceTAM [] tamInstructions
      --putStrLn $ "Final stack: " ++ show finalStack
      printf "%-10s\t%s\n" ("Final stack: ") (show finalStack)

    [file, "--run"] | takeExtension file == ".exp" -> do
      contents <- readFile file
      let ast = parseArith contents
          tamInstructions = expCode ast
      putStrLn "Running generated TAM code:"
      print (execTAM [] tamInstructions)

    [file, "--evaluate"] | takeExtension file == ".exp" -> do
      contents <- readFile file
      let ast = parseArith contents
      print $ "Evaluation result: " ++ show (evaluate ast)

    [file] -> case takeExtension file of
      ".exp" -> do
        contents <- readFile file
        let ast = parseArith contents
        let tamFile = replaceExtension file ".tam"
        writeFile tamFile (show (expCode ast))
        putStrLn $ "Compiled to " ++ tamFile

      ".tam" -> do
        contents <- readFile file
        let tamInstructions = expCode (parseArith contents)
        print $ "Execution result: " ++ show (execTAM [] tamInstructions)

    _ -> putStrLn "Usage: program <file> [--trace | --run | --evaluate]"

