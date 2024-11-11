module Compiler where

import Data.Char
import Data.List
import Debug.Trace
import Grammar
import Parser
import State
import TAM

type VarEnv = [(Identifier, Address)]

type LabelCounter = Integer

type CompilerState = (VarEnv, LabelCounter)

expCode :: Expr -> VarEnv -> [TAMInst]
expCode (LitInteger x) _ = [LOADL x]
expCode (Var var) env = [LOAD (lookupVar var env)]
expCode (BinOp Addition ast ast') env = expCode ast env ++ expCode ast' env ++ [ADD]
expCode (BinOp Subtraction ast ast') env = expCode ast env ++ expCode ast' env ++ [SUB]
expCode (BinOp Multiplication ast ast') env = expCode ast env ++ expCode ast' env ++ [MUL]
expCode (BinOp Division ast ast') env = expCode ast env ++ expCode ast' env ++ [DIV]
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

compileProgram :: Program -> [TAMInst]
compileProgram (LetIn declarations command) = evalState compileState ([], 0)
  where
    compileState = do
      declareInst <- declareVars declarations
      commandInst <- commandCode command
      return (declareInst ++ commandInst ++ [HALT])

declareVar :: Identifier -> State CompilerState [TAMInst]
declareVar var = do
  (env, labelCounter) <- get
  let addr = fromIntegral (length env)
      newEnv = (var, addr) : env
  put (newEnv, labelCounter)
  return [LOADL 0]

initVar :: Identifier -> Expr -> State CompilerState [TAMInst]
initVar var expr = do
  (env, labelCounter) <- get
  let addr = fromIntegral (length env)
      newEnv = (var, addr) : env
  put (newEnv, labelCounter)
  let exprInst = expCode expr env
  return (exprInst ++ [STORE addr])

declareVars :: [Declaration] -> State CompilerState [TAMInst]
declareVars [] = return []
declareVars ((VarDeclare var) : declarations) = do
  inst <- declareVar var
  rest <- declareVars declarations
  return (inst ++ rest)
declareVars ((VarInitialize var expr) : declarations) = do
  inst <- initVar var expr
  rest <- declareVars declarations
  return (inst ++ rest)

lookupVar :: Identifier -> VarEnv -> Address
lookupVar var env = case lookup var env of
  Just addr -> addr
  Nothing -> error ("Variable " ++ show var ++ " not declared.")

commandCode :: Command -> State CompilerState [TAMInst]
commandCode (Assignment var expr) = do
  env <- gets fst
  let exprInst = expCode expr env
  return (exprInst ++ [STORE (lookupVar var env)])
commandCode (PrintInt x) = do
  env <- gets fst
  let exprCode = expCode x env
  return (exprCode ++ [PUTINT])
commandCode (BeginEnd []) = return []
commandCode (BeginEnd (cmd : cmds)) = do
  inst <- commandCode cmd
  restInst <- commandCode (BeginEnd cmds)
  return (inst ++ restInst)
commandCode (GetInt var) = do
  env <- gets fst
  return [GETINT, STORE (lookupVar var env)]
commandCode (If cond cmd1 cmd2) = do
  env <- gets fst
  let condCode = expCode cond env
  label1 <- freshLabel
  label2 <- freshLabel
  cmd1Code <- commandCode cmd1
  cmd2Code <- commandCode cmd2
  return $
    condCode
      ++ [JUMPIFZ label1]
      ++ cmd1Code
      ++ [JUMP label2]
      ++ [LABEL label1]
      ++ cmd2Code
      ++ [LABEL label2]
commandCode (While cond cmd) = do
  env <- gets fst
  label1 <- freshLabel
  label2 <- freshLabel
  let condCode = expCode cond env
  cmdCode <- commandCode cmd
  return $
    [LABEL label1]
      ++ condCode
      ++ [JUMPIFZ label2]
      ++ cmdCode
      ++ [JUMP label1]
      ++ [LABEL label2]

freshLabel :: State CompilerState LabelID
freshLabel = do
  (env, labelCounter) <- get
  put (env, labelCounter + 1)
  return labelCounter