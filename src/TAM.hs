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
  | CALL LabelID
  | RETURN Int Int
  deriving (Read, Show, Eq)

data Address
  = SB Integer -- Stack base-relative address
  | LB Integer -- Local base-relative address
  deriving (Read, Eq)

instance Show Address where
  show (SB offset)
    | offset >= 0 = "[SB + " ++ show offset ++ "]"
    | otherwise = "[SB - " ++ show (abs offset) ++ "]"
  show (LB offset)
    | offset >= 0 = "[LB + " ++ show offset ++ "]"
    | otherwise = "[LB - " ++ show (abs offset) ++ "]"

type LabelID = String

type Stack = [Integer]

data TAMState = TAMState
  { tsCode :: [TAMInst],
    tsCounter :: Int,
    tsStack :: Stack,
    tsSB :: Integer,
    tsLB :: Integer
  }

execTAM :: [TAMInst] -> IO Stack
execTAM instructions = do
  let initialState = TAMState {tsCode = instructions, tsCounter = 0, tsStack = [], tsSB = 0, tsLB = 0}
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
    CALL labelID -> executeCall labelID
    RETURN n m -> executeReturn n m
    LABEL _ -> return ()

tsClearStack :: Int -> TAMState -> TAMState
tsClearStack m ts = ts {tsStack = drop m (tsStack ts)}

executeReturn :: Int -> Int -> StateIO TAMState ()
executeReturn n m = do
  ts <- get
  let (results, ts1) = tsPopN n ts
      (returnAddress, ts2) = tsPop ts1
      (oldLB, ts3) = tsPop ts2
      ts4 = tsClearStack m ts3
  put $ tsPushN results $ ts4 {tsCounter = fromIntegral returnAddress, tsLB = fromIntegral oldLB}

tsPopN :: Int -> TAMState -> ([Integer], TAMState)
tsPopN n ts =
  let (top, rest) = splitAt n (tsStack ts)
   in (reverse top, ts {tsStack = rest})

tsPushN :: [Integer] -> TAMState -> TAMState
tsPushN xs ts = ts {tsStack = reverse xs ++ tsStack ts}

executeCall :: LabelID -> StateIO TAMState ()
executeCall labelID = do
  ts <- get
  let returnAddress = tsCounter ts
      oldLB = tsLB ts
  put $
    tsPush (fromIntegral returnAddress) $
      tsPush (fromIntegral oldLB) $
        ts {tsLB = fromIntegral (length (tsStack ts))}
  executeJump labelID

executeJump :: LabelID -> StateIO TAMState ()
executeJump labelID = do
  ts <- get
  put $ ts {tsCounter = findLabel labelID (tsCode ts)}

parseLabel :: Parser LabelID
parseLabel = parseToken $ some (satisfy (not . isSpace))

parseTAMProgram :: Parser [TAMInst]
parseTAMProgram = some (instTAM <* parseSpace)

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
    <|> (parseToken (parseString "LOAD") >> parseAddress >>= return . LOAD)
    <|> (parseToken (parseString "STORE") >> parseAddress >>= return . STORE)
    <|> (parseToken (parseString "Label") >> parseLabel >>= return . LABEL)
    <|> (parseToken (parseString "CALL") >> parseLabel >>= return . CALL)
    <|> instReturn

instReturn :: Parser TAMInst
instReturn = do
  parseToken (parseString "RETURN")
  n <- parseInt
  RETURN (fromIntegral n) . fromIntegral <$> parseInt

parseAddress :: Parser Address
parseAddress =
  ( do
      parseToken (parseString "[SB")
      offset <- parsePositiveInt
      parseToken (parseString "]")
      return (SB offset)
  )
    <|> ( do
            parseToken (parseString "[LB")
            offset <- parseNegativeInt <|> parsePositiveInt
            parseToken (parseString "]")
            return (LB offset)
        )
    <|> ( do
            SB <$> parseInt
        )

parseNegativeInt :: Parser Integer
parseNegativeInt = do
  _ <- parseToken (parseString "-")
  n <- parseInt
  return (-n)

parsePositiveInt :: Parser Integer
parsePositiveInt = do
  _ <- parseToken (parseString "+")
  parseInt

showInst :: TAMInst -> String
showInst (LABEL lbl) = "Label " ++ lbl
showInst (JUMP lbl) = "JUMP " ++ lbl
showInst (JUMPIFZ lbl) = "JUMPIFZ " ++ lbl
showInst (CALL id) = "CALL " ++ id
showInst (RETURN n m) = "RETURN " ++ show n ++ " " ++ show m
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
  let value = load addr ts
  put $ tsPush value ts

load :: Address -> TAMState -> Integer
load (SB offset) ts = tsStack ts !! (length (tsStack ts) - 1 - fromIntegral (offset + tsSB ts))
load (LB offset) ts = tsStack ts !! (length (tsStack ts) - 1 - fromIntegral (offset + tsLB ts))

executeStore :: Address -> StateIO TAMState ()
executeStore addr = do
  ts <- get
  let (value, ts') = tsPop ts
  put $ store addr value ts'

store :: Address -> Integer -> TAMState -> TAMState
store (SB offset) newVal ts =
  let index = length (tsStack ts) - 1 - fromIntegral (offset + tsSB ts)
      newStack = replaceAt index newVal (tsStack ts)
   in ts {tsStack = newStack}
store (LB offset) newVal ts =
  let index = length (tsStack ts) - 1 - fromIntegral (offset + tsLB ts)
      newStack = replaceAt index newVal (tsStack ts)
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
  let initialState = TAMState {tsCode = instructions, tsCounter = 0, tsStack = [], tsSB = 0, tsLB = 0}
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
