module TAM where

import Data.Char
import Data.List

data TAMInst
  = LOADL Integer
  | ADD
  | SUB
  | MUL
  | DIV
  | NEG
  | MOD
  | AND
  | OR
  | NOT
  | LSS
  | GTR
  | EQL
  | HALT
  | GETINT
  | PUTINT
  | LABEL LabelID
  | JUMP LabelID
  | JUMPIFZ LabelID
  | LOAD Address
  | STORE Address
  deriving (Read, Show, Eq)

type ProgramCounter = Integer

type LabelID = Integer

type Address = Integer

type Stack = [Integer]

execute :: Stack -> TAMInst -> [TAMInst] -> ProgramCounter -> IO (Stack, ProgramCounter)
execute stack (LOADL x) _ pc = return (x : stack, pc + 1)
execute (y : x : rest) ADD _ pc = return ((x + y) : rest, pc + 1)
execute (y : x : rest) SUB _ pc = return ((x - y) : rest, pc + 1)
execute (y : x : rest) MUL _ pc = return ((x * y) : rest, pc + 1)
execute (y : x : rest) DIV _ pc = return ((x `div` y) : rest, pc + 1)
execute (x : rest) NEG _ pc = return (-x : rest, pc + 1)
execute (y : x : rest) AND _ pc = return (logicAnd x y : rest, pc + 1)
execute (y : x : rest) OR _ pc = return (logicOr x y : rest, pc + 1)
execute (x : rest) NOT _ pc = return (logicNot x : rest, pc + 1)
execute (y : x : rest) LSS _ pc = return (comparison (<) x y : rest, pc + 1)
execute (y : x : rest) GTR _ pc = return (comparison (>) x y : rest, pc + 1)
execute (y : x : rest) EQL _ pc = return (comparison (==) x y : rest, pc + 1)
execute stack (LOAD addr) _ pc = return (load addr stack : stack, pc + 1)
execute (x : rest) (STORE addr) _ pc = return (store addr x rest, pc + 1)
execute (x : rest) PUTINT _ pc = do
  putStrLn $ "Output > " ++ show x
  return (rest, pc + 1)
execute stack GETINT _ pc = do
  x <- getIntFromTerminal
  return (x : stack, pc + 1)
execute stack (JUMP labelID) instructions _ = return (stack, findLabel labelID instructions)
execute (x : stack) (JUMPIFZ labelID) instructions pc =
  if x == 0
    then return (stack, findLabel labelID instructions)
    else return (stack, pc + 1)
execute stack HALT _ _ = return (stack, -1)
execute stack (LABEL _) _ pc = return (stack, pc + 1)

logicAnd :: Integer -> Integer -> Integer
logicAnd x y = if x /= 0 && y /= 0 then 1 else 0

logicOr :: Integer -> Integer -> Integer
logicOr x y = if x /= 0 || y /= 0 then 1 else 0

logicNot :: Integer -> Integer
logicNot x = if x == 0 then 1 else 0

comparison :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer
comparison op x y = if op x y then 1 else 0

findLabel :: LabelID -> [TAMInst] -> ProgramCounter
findLabel labelID instructions = case elemIndex (LABEL labelID) instructions of
  Just index -> fromIntegral index
  Nothing -> error ("Label " ++ show labelID ++ " not found.")

getIntFromTerminal :: IO Integer
getIntFromTerminal = do
  putStrLn "Enter an number: "
  readLn

load :: Address -> Stack -> Integer
load addr stack = stack !! (length stack - 1 - fromIntegral addr)

store :: Address -> Integer -> Stack -> Stack
store addr newVal stack =
  let index = length stack - 1 - fromIntegral addr
   in take index stack ++ [newVal] ++ drop (index + 1) stack

execTAM :: Stack -> [TAMInst] -> ProgramCounter -> IO Stack
execTAM stack instructions pc
  | pc < 0 || pc >= fromIntegral (length instructions) = return stack
  | otherwise = do
      let inst = instructions !! fromIntegral pc
      (newStack, newPC) <- execute stack inst instructions pc
      if newPC == -1
        then return newStack
        else execTAM newStack instructions newPC

traceTAM :: Stack -> [TAMInst] -> IO Stack
traceTAM stack instructions = do
  putStrLn ("Initial stack: " ++ "\t\t" ++ show stack)
  traceExecTAM stack instructions 0

traceExecTAM :: Stack -> [TAMInst] -> ProgramCounter -> IO Stack
traceExecTAM stack instructions pc
  | pc < 0 || pc >= fromIntegral (length instructions) = do
      return stack
  | otherwise = do
      let inst = instructions !! fromIntegral pc
      do
        (newStack, newPC) <- execute stack inst instructions pc
        if newPC == -1
          then return newStack
          else do
            if inst == PUTINT
              then traceExecTAM newStack instructions newPC
              else do
                putStrLn $ show inst ++ "\t\t" ++ show newStack
                traceExecTAM newStack instructions newPC
