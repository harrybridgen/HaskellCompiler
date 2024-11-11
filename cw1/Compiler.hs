module Compiler where

import Data.Char
import Data.List
import Debug.Trace
import Grammar
  ( BinOperator
      ( Addition,
        Conjunction,
        Disjunction,
        Division,
        Equal,
        GreaterThan,
        GreaterThanOrEqual,
        LessThan,
        LessThanOrEqual,
        Multiplication,
        NotEqual,
        Subtraction
      ),
    Command (..),
    Declaration (..),
    Expr (..),
    Identifier,
    Program (..),
    UnOperator (Negation, Not),
  )
import Parser
import State
import TAM

type VarEnv = [(Identifier, Address)]

type LabelCounter = Integer

type CompilerState = (VarEnv, LabelCounter)

compileProgram :: Program -> [TAMInst]
compileProgram (LetIn declarations command) = evalState compileState ([], 0)
  where
    compileState = do
      declareInst <- declareVars declarations
      commandInst <- commandCode command
      return (declareInst ++ commandInst ++ [HALT])

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

commandCode :: Command -> State CompilerState [TAMInst]
commandCode (Assignment var expr) = compileAssignment var expr
commandCode (PrintInt x) = compilePrint x
commandCode (BeginEnd cmds) = compileBeginEnd cmds
commandCode (GetInt var) = compileGetInt var
commandCode (If cond cmd1 cmd2) = compileIf cond cmd1 cmd2
commandCode (While cond cmd) = compileWhile cond cmd

compileAssignment :: Identifier -> Expr -> State CompilerState [TAMInst]
compileAssignment var expr = do
  env <- gets fst
  let exprInst = expCode expr env
  return (exprInst ++ [STORE (lookupVar var env)])

compilePrint :: Expr -> State CompilerState [TAMInst]
compilePrint expr = do
  env <- gets fst
  let exprInst = expCode expr env
  return (exprInst ++ [PUTINT])

compileBeginEnd :: [Command] -> State CompilerState [TAMInst]
compileBeginEnd [] = return []
compileBeginEnd (cmd : cmds) = do
  inst <- commandCode cmd
  restInst <- compileBeginEnd cmds
  return (inst ++ restInst)

compileGetInt :: Identifier -> State CompilerState [TAMInst]
compileGetInt var = do
  env <- gets fst
  return [GETINT, STORE (lookupVar var env)]

compileIf :: Expr -> Command -> Command -> State CompilerState [TAMInst]
compileIf cond cmd1 cmd2 = do
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

compileWhile :: Expr -> Command -> State CompilerState [TAMInst]
compileWhile cond cmd = do
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

lookupVar :: Identifier -> VarEnv -> Address
lookupVar var env = case lookup var env of
  Just addr -> addr
  Nothing -> error ("Variable " ++ show var ++ " not declared.")

freshLabel :: State CompilerState LabelID
freshLabel = do
  (env, labelCounter) <- get
  put (env, labelCounter + 1)
  return labelCounter

expCode :: Expr -> VarEnv -> [TAMInst]
expCode (LitInteger x) _ = expCodeLitInteger x
expCode (Var var) env = expCodeVar var env
expCode (BinOp op ast ast') env = expCodeBinOp op ast ast' env
expCode (UnOp op ast) env = expCodeUnOp op ast env
expCode (Conditional b x y) env = expCodeConditional b x y env

expCodeLitInteger :: Integer -> [TAMInst]
expCodeLitInteger x = [LOADL x]

expCodeVar :: Identifier -> VarEnv -> [TAMInst]
expCodeVar var env = [LOAD (lookupVar var env)]

expCodeBinOp :: BinOperator -> Expr -> Expr -> VarEnv -> [TAMInst]
expCodeBinOp Addition ast ast' env = expCode ast env ++ expCode ast' env ++ [ADD]
expCodeBinOp Subtraction ast ast' env = expCode ast env ++ expCode ast' env ++ [SUB]
expCodeBinOp Multiplication ast ast' env = expCode ast env ++ expCode ast' env ++ [MUL]
expCodeBinOp Division ast ast' env = expCode ast env ++ expCode ast' env ++ [DIV]
expCodeBinOp Conjunction ast ast' env = expCode ast env ++ expCode ast' env ++ [AND]
expCodeBinOp Disjunction ast ast' env = expCode ast env ++ expCode ast' env ++ [OR]
expCodeBinOp LessThan ast ast' env = expCode ast env ++ expCode ast' env ++ [LSS]
expCodeBinOp GreaterThan ast ast' env = expCode ast env ++ expCode ast' env ++ [GTR]
expCodeBinOp Equal ast ast' env = expCode ast env ++ expCode ast' env ++ [EQL]
expCodeBinOp LessThanOrEqual ast1 ast2 env =
  expCode ast1 env
    ++ expCode ast2 env
    ++ [LSS]
    ++ expCode ast1 env
    ++ expCode ast2 env
    ++ [EQL]
    ++ [OR]
expCodeBinOp GreaterThanOrEqual ast1 ast2 env =
  expCode ast1 env
    ++ expCode ast2 env
    ++ [GTR]
    ++ expCode ast1 env
    ++ expCode ast2 env
    ++ [EQL]
    ++ [OR]
expCodeBinOp NotEqual ast1 ast2 env = expCode ast1 env ++ expCode ast2 env ++ [EQL, NOT]

expCodeUnOp :: UnOperator -> Expr -> VarEnv -> [TAMInst]
expCodeUnOp Negation ast env = expCode ast env ++ [NEG]
expCodeUnOp Not ast env = expCode ast env ++ [NOT]

expCodeConditional :: Expr -> Expr -> Expr -> VarEnv -> [TAMInst]
expCodeConditional b x y env =
  expCode b env
    ++ expCode x env
    ++ [MUL]
    ++ [LOADL 1]
    ++ expCode b env
    ++ [SUB]
    ++ expCode y env
    ++ [MUL]
    ++ [ADD]
