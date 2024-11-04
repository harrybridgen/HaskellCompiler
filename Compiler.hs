module Compiler where

import Data.Char
import Data.List
import Grammar
import Parser
import TAM

type VarEnv = [(Identifier, Address)]

type LabelCounter = Integer

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
