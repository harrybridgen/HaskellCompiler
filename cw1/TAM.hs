module TAM where

import Control.Applicative
import Data.Char
import Data.List
import Parser
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
  | GRT
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
    GRT -> executeBinaryOp (comparison (>))
    EQL -> executeBinaryOp (comparison (==))
    LOAD addr -> executeLoad addr
    STORE addr -> executeStore addr
    PUTINT -> executePutInt
    GETINT -> executeGetInt
    JUMP labelID -> executeJump labelID
    JUMPIFZ labelID -> executeJumpIfZero labelID
    HALT -> executeHalt
    LABEL _ -> return ()

parseTAMProgram :: Parser [TAMInst]
parseTAMProgram = some (instTAM <* parseSpace)

parseLabel :: Parser LabelID
parseLabel = parseToken $ some (satisfy (not . isSpace))

instLOADL :: Parser TAMInst
instLOADL = do
  parseToken (parseString "LOADL")
  LOADL <$> parseInt

instTAM :: Parser TAMInst
instTAM =
  (parseToken (parseString "HALT") >> return HALT)
    <|> instLOADL
    <|> (parseToken (parseString "ADD") >> return ADD)
    <|> (parseToken (parseString "SUB") >> return SUB)
    <|> (parseToken (parseString "MUL") >> return MUL)
    <|> (parseToken (parseString "DIV") >> return DIV)
    <|> (parseToken (parseString "NEG") >> return NEG)
    <|> (parseToken (parseString "AND") >> return AND)
    <|> (parseToken (parseString "OR") >> return OR)
    <|> (parseToken (parseString "NOT") >> return NOT)
    <|> (parseToken (parseString "LSS") >> return LSS)
    <|> (parseToken (parseString "GRT") >> return GRT)
    <|> (parseToken (parseString "EQL") >> return EQL)
    <|> (parseToken (parseString "GETINT") >> return GETINT)
    <|> (parseToken (parseString "PUTINT") >> return PUTINT)
    <|> (parseToken (parseString "JUMPIFZ") >> parseLabel >>= return . JUMPIFZ)
    <|> (parseToken (parseString "JUMP") >> parseLabel >>= return . JUMP)
    <|> (parseToken (parseString "LOAD") >> parseInt >>= return . LOAD)
    <|> (parseToken (parseString "STORE") >> parseInt >>= return . STORE)
    <|> (parseToken (parseString "Label") >> parseLabel >>= return . LABEL)

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
  lift $ putStrLn $ "Storing " ++ show x ++ " at " ++ show addr ++ ". " ++ "Stack: " ++ show (tsStack ts')
  put $ store addr x ts'

store :: Address -> Integer -> TAMState -> TAMState
store addr newVal ts =
  let stack = tsStack ts
      index = length stack - 1 - fromIntegral addr
      newStack = replaceAt index newVal stack
   in ts {tsStack = newStack}

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs =
  let before = take idx xs
      after = tail (drop idx xs)
   in before ++ [val] ++ after

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
