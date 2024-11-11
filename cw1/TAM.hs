module TAM where

import Data.Char
import Data.List
import StateIO

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

type LabelID = String

type Address = Integer

type Stack = [Integer]

data TAMState = TAMState
  { tsCode :: [TAMInst],
    tsCounter :: Int,
    tsStack :: Stack
  }

execTAM :: [TAMInst] -> IO Stack
execTAM instructions = do
  let initialState = TAMState {tsCode = instructions, tsCounter = 0, tsStack = []}
  (_, finalState) <- runStateIO runProgram initialState
  return $ tsStack finalState

runProgram :: StateIO TAMState ()
runProgram = do
  ts <- get
  if tsCounter ts < 0 || tsCounter ts >= length (tsCode ts)
    then return ()
    else do
      let inst = tsInstruction ts
      execute inst
      runProgram

tsInstruction :: TAMState -> TAMInst
tsInstruction ts = tsCode ts !! tsCounter ts

execute :: TAMInst -> StateIO TAMState ()
execute inst = do
  modify tsIncrementCounter
  ts <- get
  case inst of
    LOADL x -> put $ tsPush x ts
    ADD -> executeBinaryOp (+)
    SUB -> executeBinaryOp (-)
    MUL -> executeBinaryOp (*)
    DIV -> executeBinaryOp div
    NEG -> executeUnaryOp negate
    AND -> executeBinaryOp logicAnd
    OR -> executeBinaryOp logicOr
    NOT -> executeUnaryOp logicNot
    LSS -> executeBinaryOp (comparison (<))
    GTR -> executeBinaryOp (comparison (>))
    EQL -> executeBinaryOp (comparison (==))
    LOAD addr -> executeLoad addr
    STORE addr -> executeStore addr
    PUTINT -> executePutInt
    GETINT -> executeGetInt
    JUMP labelID -> executeJump labelID
    JUMPIFZ labelID -> executeJumpIfZero labelID
    HALT -> executeHalt
    LABEL _ -> return ()

readInst :: String -> TAMInst
readInst input = case words input of
  ["LOADL", x] -> LOADL (read x)
  ["ADD"] -> ADD
  ["SUB"] -> SUB
  ["MUL"] -> MUL
  ["DIV"] -> DIV
  ["NEG"] -> NEG
  ["MOD"] -> MOD
  ["AND"] -> AND
  ["OR"] -> OR
  ["NOT"] -> NOT
  ["LSS"] -> LSS
  ["GTR"] -> GTR
  ["EQL"] -> EQL
  ["HALT"] -> HALT
  ["GETINT"] -> GETINT
  ["PUTINT"] -> PUTINT
  ["Label", labelID] -> LABEL labelID
  ["JUMP", labelID] -> JUMP labelID
  ["JUMPIFZ", labelID] -> JUMPIFZ labelID
  ["LOAD", addr] -> LOAD (read addr)
  ["STORE", addr] -> STORE (read addr)
  _ -> error ("Invalid instruction: " ++ input)

showInst :: TAMInst -> String
showInst (LABEL lbl) = "Label " ++ lbl
showInst (JUMP lbl) = "JUMP " ++ lbl
showInst (JUMPIFZ lbl) = "JUMPIFZ " ++ lbl
showInst inst = show inst

executeBinaryOp :: (Integer -> Integer -> Integer) -> StateIO TAMState ()
executeBinaryOp op = do
  ts <- get
  let (y, ts') = tsPop ts
  let (x, ts'') = tsPop ts'
  put $ tsPush (op x y) ts''

executeUnaryOp :: (Integer -> Integer) -> StateIO TAMState ()
executeUnaryOp op = do
  ts <- get
  let (x, ts') = tsPop ts
  put $ tsPush (op x) ts'

executeLoad :: Address -> StateIO TAMState ()
executeLoad addr = do
  ts <- get
  put $ tsPush (load addr (tsStack ts)) ts

load :: Address -> Stack -> Integer
load addr stack = stack !! (length stack - 1 - fromIntegral addr)

executeStore :: Address -> StateIO TAMState ()
executeStore addr = do
  ts <- get
  let (x, ts') = tsPop ts
  put $ store addr x ts'

store :: Address -> Integer -> TAMState -> TAMState
store addr newVal ts =
  let stack = tsStack ts
      index = length stack - 1 - fromIntegral addr
      newStack = take index stack ++ [newVal] ++ drop (index + 1) stack
   in ts {tsStack = newStack}

executePutInt :: StateIO TAMState ()
executePutInt = do
  ts <- get
  let (x, ts') = tsPop ts
  lift $ putStrLn $ "Output > " ++ show x
  put ts'

executeGetInt :: StateIO TAMState ()
executeGetInt = do
  x <- lift getIntFromTerminal
  ts <- get
  put $ tsPush x ts

executeJump :: LabelID -> StateIO TAMState ()
executeJump labelID = do
  ts <- get
  put $ ts {tsCounter = findLabel labelID (tsCode ts)}

executeJumpIfZero :: LabelID -> StateIO TAMState ()
executeJumpIfZero labelID = do
  ts <- get
  let (x, ts') = tsPop ts
  if x == 0
    then put $ ts' {tsCounter = findLabel labelID (tsCode ts')}
    else put ts'

executeHalt :: StateIO TAMState ()
executeHalt = do
  ts <- get
  put $ tsSetCounter (-1) ts

tsSetCounter :: Int -> TAMState -> TAMState
tsSetCounter i ts = ts {tsCounter = i}

tsIncrementCounter :: TAMState -> TAMState
tsIncrementCounter ts = ts {tsCounter = tsCounter ts + 1}

tsPush :: Integer -> TAMState -> TAMState
tsPush x ts = ts {tsStack = x : tsStack ts}

tsPop :: TAMState -> (Integer, TAMState)
tsPop ts = let (x : xs) = tsStack ts in (x, ts {tsStack = xs})

logicAnd :: Integer -> Integer -> Integer
logicAnd x y = if x /= 0 && y /= 0 then 1 else 0

logicOr :: Integer -> Integer -> Integer
logicOr x y = if x /= 0 || y /= 0 then 1 else 0

logicNot :: Integer -> Integer
logicNot x = if x == 0 then 1 else 0

comparison :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer
comparison op x y = if op x y then 1 else 0

findLabel :: LabelID -> [TAMInst] -> Int
findLabel labelID instructions = case elemIndex (LABEL labelID) instructions of
  Just index -> index
  Nothing -> error ("Label " ++ labelID ++ " not found.")

getIntFromTerminal :: IO Integer
getIntFromTerminal = do
  putStrLn "Enter a number: "
  readLn

-- Functions for testing in GHCi
traceTAM :: [TAMInst] -> IO Stack
traceTAM instructions = do
  let initialState = TAMState {tsCode = instructions, tsCounter = 0, tsStack = []}
  (_, finalState) <- runStateIO traceProgram initialState
  return $ tsStack finalState

traceProgram :: StateIO TAMState ()
traceProgram = do
  ts <- get
  if tsCounter ts < 0 || tsCounter ts >= length (tsCode ts)
    then return ()
    else do
      let inst = tsInstruction ts
      execute inst
      ts' <- get
      lift $ putStrLn $ showInst inst ++ "\t\t" ++ show (tsStack ts')
      traceProgram
