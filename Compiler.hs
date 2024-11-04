module Compiler where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Grammar
import Parser

type Stack = [Integer]

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

execute :: TAMInst -> Stack -> [TAMInst] -> ProgramCounter -> IO (Stack, ProgramCounter)
execute (LOADL x) stack _ pc = return (x : stack, pc + 1)
execute ADD (y : x : rest) _ pc = return ((x + y) : rest, pc + 1)
execute SUB (y : x : rest) _ pc = return ((x - y) : rest, pc + 1)
execute MUL (y : x : rest) _ pc = return ((x * y) : rest, pc + 1)
execute DIV (y : x : rest) _ pc = return ((x `div` y) : rest, pc + 1)
execute MOD (y : x : rest) _ pc = return ((x `mod` y) : rest, pc + 1)
execute NEG (x : rest) _ pc = return (-x : rest, pc + 1)
execute AND (y : x : rest) _ pc = return (logicAnd x y : rest, pc + 1)
execute OR (y : x : rest) _ pc = return (logicOr x y : rest, pc + 1)
execute NOT (x : rest) _ pc = return (logicNot x : rest, pc + 1)
execute LSS (y : x : rest) _ pc = return (comparison (<) x y : rest, pc + 1)
execute GTR (y : x : rest) _ pc = return (comparison (>) x y : rest, pc + 1)
execute EQL (y : x : rest) _ pc = return (comparison (==) x y : rest, pc + 1)
execute (LOAD addr) stack _ pc = return (load addr stack : stack, pc + 1)
execute (STORE addr) (x : rest) _ pc = return (store addr x rest, pc + 1)
execute PUTINT (x : rest) _ pc = do
  putStrLn $ "Output > " ++ show x
  return (rest, pc + 1)
execute GETINT stack _ pc = do
  x <- getIntFromTerminal
  return (x : stack, pc + 1)
execute (JUMP labelID) stack instructions _ = return (stack, findLabel labelID instructions)
execute (JUMPIFZ labelID) (x : stack) instructions pc =
  if x == 0
    then return (stack, findLabel labelID instructions)
    else return (stack, pc + 1)
execute HALT stack _ _ = return (stack, -1)
execute (LABEL _) stack _ pc = return (stack, pc + 1)

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

execTAM :: Stack -> [TAMInst] -> ProgramCounter -> IO Stack
execTAM stack instructions pc
  | pc < 0 || pc >= fromIntegral (length instructions) = return stack
  | otherwise = do
      let inst = instructions !! fromIntegral pc
      (newStack, newPC) <- execute inst stack instructions pc
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
        (newStack, newPC) <- execute inst stack instructions pc
        if newPC == -1
          then return newStack
          else do
            if inst == PUTINT
              then traceExecTAM newStack instructions newPC
              else do
                putStrLn $ show inst ++ "\t\t" ++ show newStack
                traceExecTAM newStack instructions newPC

expCode :: Expr -> VarEnv -> [TAMInst]
expCode (LitInteger x) _ = [LOADL x]
expCode (Var var) env = [LOAD (lookupVar var env)]
expCode (BinOp Addition ast ast') env = expCode ast env ++ expCode ast' env ++ [ADD]
expCode (BinOp Subtraction ast ast') env = expCode ast env ++ expCode ast' env ++ [SUB]
expCode (BinOp Multiplication ast ast') env = expCode ast env ++ expCode ast' env ++ [MUL]
expCode (BinOp Division ast ast') env = expCode ast env ++ expCode ast' env ++ [DIV]
expCode (BinOp Mod ast ast') env = expCode ast env ++ expCode ast' env ++ [MOD]
expCode (BinOp Conjunction ast ast') env = expCode ast env ++ expCode ast' env ++ [AND]
expCode (BinOp Disjunction ast ast') env = expCode ast env ++ expCode ast' env ++ [OR]
expCode (UnOp Negation ast) env = expCode ast env ++ [NEG]
expCode (UnOp Not ast) env = expCode ast env ++ [NOT]
expCode (BinOp LessThan ast ast') env = expCode ast env ++ expCode ast' env ++ [LSS]
expCode (BinOp GreaterThan ast ast') env = expCode ast env ++ expCode ast' env ++ [GTR]
expCode (BinOp Equal ast ast') env = expCode ast env ++ expCode ast' env ++ [EQL]
expCode (BinOp LessThanOrEqual ast1 ast2) env =
  expCode ast1 env
    ++ expCode ast2 env
    ++ [LSS]
    ++ expCode ast1 env
    ++ expCode ast2 env
    ++ [EQL]
    ++ [OR]
expCode (BinOp GreaterThanOrEqual ast1 ast2) env =
  expCode ast1 env
    ++ expCode ast2 env
    ++ [GTR]
    ++ expCode ast1 env
    ++ expCode ast2 env
    ++ [EQL]
    ++ [OR]
expCode (BinOp NotEqual ast1 ast2) env =
  expCode ast1 env
    ++ expCode ast2 env
    ++ [EQL]
    ++ [NOT]
expCode (Conditional b x y) env =
  expCode b env
    ++ expCode x env
    ++ [MUL]
    ++ [LOADL 1]
    ++ expCode b env
    ++ [SUB]
    ++ expCode y env
    ++ [MUL]
    ++ [ADD]

compArith :: String -> [TAMInst]
compArith expression = case parse parseExpr expression of
  [(ast, "")] -> expCode ast []
  _ -> error "Compilation error"

stringTraceTAM :: String -> IO Stack
stringTraceTAM expression = do
  let instructions = compArith expression
  traceTAM [] instructions

type Address = Integer

type VarEnv = [(Identifier, Address)]

type LabelCounter = Integer

programCode :: Program -> [TAMInst]
programCode (LetIn declarations command) =
  let (declareInst, varEnv) = declareVars declarations []
      (commandInst, _) = commandCode command varEnv 0
   in declareInst ++ commandInst ++ [HALT]

declareVars :: [Declaration] -> VarEnv -> ([TAMInst], VarEnv)
declareVars [] varEnv = ([], varEnv)
declareVars ((VarDeclare var) : declarations) varEnv =
  let address = fromIntegral (length varEnv)
      loadInstr = [LOADL 0]
      updatedEnv = (var, address) : varEnv
      (restInstr, finalEnv) = declareVars declarations updatedEnv
   in (loadInstr ++ restInstr, finalEnv)
declareVars ((VarInitialize var expr) : declarations) varEnv =
  let address = fromIntegral (length varEnv)
      exprCode = expCode expr
      initInstr = exprCode varEnv
      updatedEnv = (var, address) : varEnv
      (restInstr, finalEnv) = declareVars declarations updatedEnv
   in (initInstr ++ restInstr, finalEnv)

load :: Address -> Stack -> Integer
load addr stack = stack !! (length stack - 1 - fromIntegral addr)

store :: Address -> Integer -> Stack -> Stack
store addr newVal stack =
  let index = length stack - 1 - fromIntegral addr
   in take index stack ++ [newVal] ++ drop (index + 1) stack

lookupVar :: Identifier -> VarEnv -> Address
lookupVar var env = case lookup var env of
  Just addr -> addr
  Nothing -> error ("Variable " ++ show var ++ " not declared.")

commandCode :: Command -> VarEnv -> LabelCounter -> ([TAMInst], LabelCounter)
commandCode (Assignment var expr) env counter =
  (expCode expr env ++ [STORE (lookupVar var env)], counter)
commandCode (PrintInt x) env counter =
  let exprCode = expCode x env
   in (exprCode ++ [PUTINT], counter)
commandCode (BeginEnd []) _ counter = ([], counter)
commandCode (BeginEnd (cmd : cmds)) env counter =
  let (inst, newCounter) = commandCode cmd env counter
      (restInst, finalCounter) = commandCode (BeginEnd cmds) env newCounter
   in (inst ++ restInst, finalCounter)
commandCode (GetInt var) env counter =
  ([GETINT, STORE (lookupVar var env)], counter)
commandCode (If cond cmd1 cmd2) env counter =
  let condCode = expCode cond env
      elseLabel = counter
      (cmd1Code, counter1) = commandCode cmd1 env (counter + 1)
      (cmd2Code, counter2) = commandCode cmd2 env counter1
      endLabel = counter2
   in ( condCode
          ++ [JUMPIFZ elseLabel]
          ++ cmd1Code
          ++ [JUMP endLabel]
          ++ [LABEL elseLabel]
          ++ cmd2Code
          ++ [LABEL endLabel],
        endLabel + 1
      )
commandCode (While cond cmd) env counter =
  let whileLabel = counter
      endLabel = counter + 1
      condCode = expCode cond env
      (cmdCode, counter1) = commandCode cmd env (counter + 2)
   in ( [LABEL whileLabel]
          ++ condCode
          ++ [JUMPIFZ endLabel]
          ++ cmdCode
          ++ [JUMP whileLabel]
          ++ [LABEL endLabel],
        counter1
      )
