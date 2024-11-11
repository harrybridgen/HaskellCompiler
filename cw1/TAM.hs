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

type LabelID = Integer

type Address = Integer

type Stack = [Integer]

data TAMState = TAMState
  { tsCode :: [TAMInst],
    tsCounter :: Int,
    tsStack :: Stack
  }

tsPush :: Integer -> TAMState -> TAMState
tsPush x ts = ts {tsStack = x : tsStack ts}

tsPop :: TAMState -> (Integer, TAMState)
tsPop ts = let (x : xs) = tsStack ts in (x, ts {tsStack = xs})

tsSetCounter :: Int -> TAMState -> TAMState
tsSetCounter i ts = ts {tsCounter = i}

tsInstruction :: TAMState -> TAMInst
tsInstruction ts = tsCode ts !! tsCounter ts

tsIncrementCounter :: TAMState -> TAMState
tsIncrementCounter ts = ts {tsCounter = tsCounter ts + 1}

execute :: TAMInst -> StateIO TAMState ()
execute inst = do
  ts <- get
  case inst of
    LOADL x -> do
      put $ tsPush x ts
      modify tsIncrementCounter
    ADD -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (x + y) ts''
      modify tsIncrementCounter
    SUB -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (x - y) ts''
      modify tsIncrementCounter
    MUL -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (x * y) ts''
      modify tsIncrementCounter
    DIV -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (x `div` y) ts''
      modify tsIncrementCounter
    NEG -> do
      let (x, ts') = tsPop ts
      put $ tsPush (-x) ts'
      modify tsIncrementCounter
    AND -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (logicAnd x y) ts''
      modify tsIncrementCounter
    OR -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (logicOr x y) ts''
      modify tsIncrementCounter
    NOT -> do
      let (x, ts') = tsPop ts
      put $ tsPush (logicNot x) ts'
      modify tsIncrementCounter
    LSS -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (comparison (<) x y) ts''
      modify tsIncrementCounter
    GTR -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (comparison (>) x y) ts''
      modify tsIncrementCounter
    EQL -> do
      let (y, ts') = tsPop ts
      let (x, ts'') = tsPop ts'
      put $ tsPush (comparison (==) x y) ts''
      modify tsIncrementCounter
    LOAD addr -> do
      put $ tsPush (load addr (tsStack ts)) ts
      modify tsIncrementCounter
    STORE addr -> do
      let (x, ts') = tsPop ts
      put $ store addr x ts'
      modify tsIncrementCounter
    PUTINT -> do
      let (x, ts') = tsPop ts
      lift $ putStrLn $ "Output > " ++ show x
      put ts'
      modify tsIncrementCounter
    GETINT -> do
      x <- lift getIntFromTerminal
      modify $ tsPush x
      modify tsIncrementCounter
    JUMP labelID -> do
      put $ ts {tsCounter = findLabel labelID (tsCode ts)}
    JUMPIFZ labelID -> do
      let (x, ts') = tsPop ts
      if x == 0
        then put $ ts' {tsCounter = findLabel labelID (tsCode ts')}
        else put ts'
      modify tsIncrementCounter
    HALT -> do
      put $ ts {tsCounter = -1}
    LABEL _ -> modify tsIncrementCounter

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
  Nothing -> error ("Label " ++ show labelID ++ " not found.")

getIntFromTerminal :: IO Integer
getIntFromTerminal = do
  putStrLn "Enter an number: "
  readLn

load :: Address -> Stack -> Integer
load addr stack = stack !! (length stack - 1 - fromIntegral addr)

store :: Address -> Integer -> TAMState -> TAMState
store addr newVal ts =
  let stack = tsStack ts
      index = length stack - 1 - fromIntegral addr
      newStack = take index stack ++ [newVal] ++ drop (index + 1) stack
   in ts {tsStack = newStack}

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
      lift $ putStrLn $ show inst ++ "\t\t" ++ show (tsStack ts')
      traceProgram
