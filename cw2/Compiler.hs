module Compiler where

import Data.Char
import Data.List
import Debug.Trace (trace)
import Grammar
import Parser
import State
import TAM
import TypeChecker

type VarEnv = [(Identifier, Address)]

type LabelCounter = Integer

type CompilerState = (VarEnv, LabelCounter)

compileProgram :: Program -> Either String [TAMInst]
compileProgram program =
  let (errors, _) = runState (checkProgram program) []
   in if null errors
        then Right (generateTAM program)
        else Left (unlines ("Type errors:" : errors))

generateTAM :: Program -> [TAMInst]
generateTAM (LetIn declarations command) = evalState compileState ([], 0)
  where
    compileState = do
      declareInst <- declareVars declarations
      functionInsts <- declareFuncts declarations
      commandInst <- commandCode command
      return (declareInst ++ commandInst ++ [HALT] ++ functionInsts)

declareVars :: [Declaration] -> State CompilerState [TAMInst]
declareVars [] = return []
declareVars ((VarDeclare var t) : declarations) = do
  inst <- declareVar var
  rest <- declareVars declarations
  return (inst ++ rest)
declareVars ((VarInitialize var t expr) : declarations) = do
  inst <- initVar var expr
  rest <- declareVars declarations
  return (inst ++ rest)
declareVars (_ : declarations) = declareVars declarations

declareFuncts :: [Declaration] -> State CompilerState [TAMInst]
declareFuncts [] = return []
declareFuncts ((FunDefine fun args t expr) : declarations) = do
  inst <- declareFun fun args t expr
  rest <- declareFuncts declarations
  return (inst ++ rest)
declareFuncts (_ : declarations) = declareFuncts declarations

declareFun :: Identifier -> [(Identifier, Type)] -> Type -> Expr -> State CompilerState [TAMInst]
declareFun fun args t expr = do
  (env, labelCounter) <- get
  let newEnv = (fun, LB (fromIntegral (length env))) : env
  put (newEnv, labelCounter)
  funCode fun args t expr

funCode :: Identifier -> [(Identifier, Type)] -> Type -> Expr -> State CompilerState [TAMInst]
funCode fun args returnType expr = do
  bodyCode <- expCode expr argEnv
  return $
    [LABEL fun]
      ++ bodyCode
      ++ [RETURN 1 (length args)]
  where
    argEnv = zip (map fst (reverse args)) (map (\n -> LB (-(fromIntegral n))) [1 .. length args])

declareVar :: Identifier -> State CompilerState [TAMInst]
declareVar var = do
  (env, labelCounter) <- get
  let addr = SB (fromIntegral (length env))
      newEnv = (var, addr) : env
  put (newEnv, labelCounter)
  return [LOADL 0]

initVar :: Identifier -> Expr -> State CompilerState [TAMInst]
initVar var expr = do
  (env, labelCounter) <- get
  let addr = SB (fromIntegral (length env))
      newEnv = (var, addr) : env
  put (newEnv, labelCounter)
  expCode expr env

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
  exprInst <- expCode expr env
  return (exprInst ++ [STORE (lookupVar var env)])

compilePrint :: Expr -> State CompilerState [TAMInst]
compilePrint expr = do
  env <- gets fst
  exprInst <- expCode expr env
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
  condCode <- expCode cond env
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
  condCode <- expCode cond env
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
  let newLabel = "#" ++ show labelCounter
  return newLabel

expCode :: Expr -> VarEnv -> State CompilerState [TAMInst]
expCode (LitInteger x) _ = return $ expCodeLitInteger x
expCode (LitBoolean bool) _ = return $ expCodeLitBoolean bool
expCode (Var var) env = return $ expCodeVar var env
expCode (BinOp op ast ast') env = do
  left <- expCode ast env
  right <- expCode ast' env
  return $ left ++ right ++ expCodeBinOp op
expCode (UnOp op ast) env = do
  expr <- expCode ast env
  return $ expr ++ expCodeUnOp op
expCode (Conditional b x y) env = expCodeConditional b x y env
expCode (FunCall fun args) env = expCodeFunCall fun args env

expCodeFunCall :: Identifier -> [Expr] -> VarEnv -> State CompilerState [TAMInst]
expCodeFunCall fun args env = do
  argCodes <- mapM (`expCode` env) args
  return $ concat argCodes ++ [CALL fun]

expCodeLitBoolean :: Bool -> [TAMInst]
expCodeLitBoolean True = [LOADL 1]
expCodeLitBoolean False = [LOADL 0]

expCodeLitInteger :: Integer -> [TAMInst]
expCodeLitInteger x = [LOADL x]

expCodeVar :: Identifier -> VarEnv -> [TAMInst]
expCodeVar var env = [LOAD (lookupVar var env)]

expCodeBinOp :: BinOperator -> [TAMInst]
expCodeBinOp Addition = [ADD]
expCodeBinOp Subtraction = [SUB]
expCodeBinOp Multiplication = [MUL]
expCodeBinOp Division = [DIV]
expCodeBinOp Conjunction = [AND]
expCodeBinOp Disjunction = [OR]
expCodeBinOp LessThan = [LSS]
expCodeBinOp GreaterThan = [GRT]
expCodeBinOp Equal = [EQL]
expCodeBinOp LessThanOrEqual = [GRT, NOT]
expCodeBinOp GreaterThanOrEqual = [LSS, NOT]
expCodeBinOp NotEqual = [EQL, NOT]

expCodeUnOp :: UnOperator -> [TAMInst]
expCodeUnOp Negation = [NEG]
expCodeUnOp Not = [NOT]

expCodeConditional :: Expr -> Expr -> Expr -> VarEnv -> State CompilerState [TAMInst]
expCodeConditional b x y env = do
  elseLabel <- freshLabel
  endLabel <- freshLabel
  condCode <- expCode b env
  trueBranch <- expCode x env
  falseBranch <- expCode y env
  return $
    condCode
      ++ [JUMPIFZ elseLabel]
      ++ trueBranch
      ++ [JUMP endLabel]
      ++ [LABEL elseLabel]
      ++ falseBranch
      ++ [LABEL endLabel]
