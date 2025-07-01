module TypeChecker where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Debug.Trace (trace)
import Grammar
import State

reservedKeywords :: [String]
reservedKeywords =
  [ "let",
    "in",
    "var",
    "fun",
    "if",
    "then",
    "else",
    "while",
    "do",
    "begin",
    "end",
    "true",
    "false",
    "Integer",
    "Boolean",
    "printint",
    "getint"
  ]

type Error = String

data VFType = VarType Type | FunType [Type] Type
  deriving (Eq, Show)

type VarContext = [(Identifier, VFType)]

checkProgram :: Program -> State VarContext [Error]
checkProgram (LetIn decls cmd) = do
  declErrors <- checkDecls decls
  varContext <- get
  cmdErrors <- checkCommand cmd
  return (declErrors ++ cmdErrors)

checkDecls :: [Declaration] -> State VarContext [Error]
checkDecls [] = return []
checkDecls (FunDefine identifier args returnType body : decls) = do
  varContext <- get
  let argTypes = map snd args
  let funType = FunType argTypes returnType

  -- Add the function to the global context
  if any ((== identifier) . fst) varContext
    then return ["Function " ++ identifier ++ ": Identifier declared more than once"]
    else do
      put ((identifier, funType) : varContext)
      return []

  -- Check the function definition
  errors <- checkFunDefine identifier args returnType body

  -- Continue with other declarations
  restErrors <- checkDecls decls
  return (errors ++ restErrors)
checkDecls (VarDeclare identifier type1 : decls) = do
  errors <- checkVarDeclare identifier type1
  restErrors <- checkDecls decls
  return (errors ++ restErrors)
checkDecls (VarInitialize identifier type1 expr : decls) = do
  errors <- checkVarInitialize identifier type1 expr
  restErrors <- checkDecls decls
  return (errors ++ restErrors)

checkFunDefine :: Identifier -> [(Identifier, Type)] -> Type -> Expr -> State VarContext [Error]
checkFunDefine identifier args returnType body = do
  varContext <- get

  -- Build a context for the function arguments (local scope)
  let argContext = map (\(argName, argType) -> (argName, VarType argType)) args

  -- Check for reserved keywords
  let reservedError =
        ["Function " ++ identifier ++ ": Identifier cannot be a reserved keyword" | identifier `elem` reservedKeywords]

  -- Check for duplicate argument declarations (only within function scope)
  let duplicateArgError =
        ["Function " ++ identifier ++ ": Argument names must be unique" | length (nub (map fst args)) /= length args]

  -- Create a combined local context for the function (parameters + global)
  let localContext = argContext ++ varContext

  -- Type-check the function body
  let bodyType = checkExpr localContext body

  -- Check for type mismatches in the function body
  let bodyError =
        case bodyType of
          Right t | t /= returnType -> ["Function " ++ identifier ++ ": Body type does not match return type."]
          Left err -> ["Function " ++ identifier ++ ": " ++ err]
          _ -> []

  -- Return all errors found
  return (reservedError ++ duplicateArgError ++ bodyError)

checkVarDeclare :: Identifier -> Type -> State VarContext [Error]
checkVarDeclare identifier type1 = do
  varContext <- get
  let reservedError =
        ["Variable " ++ identifier ++ ": Identifier cannot be a reserved keyword" | identifier `elem` reservedKeywords]
  let duplicateError =
        ["Variable " ++ identifier ++ ": Identifier declared more than once" | any ((== identifier) . fst) varContext]
  put ((identifier, VarType type1) : varContext)
  return (reservedError ++ duplicateError)

checkVarInitialize :: Identifier -> Type -> Expr -> State VarContext [Error]
checkVarInitialize identifier type1 expr = do
  varContext <- get
  let reservedError =
        ["Variable " ++ identifier ++ ": Identifier cannot be a reserved keyword" | identifier `elem` reservedKeywords]
  let duplicateError =
        ["Variable " ++ identifier ++ ": Identifier declared more than once" | any ((== identifier) . fst) varContext]
  let exprType = checkExpr varContext expr
  let typeError =
        case exprType of
          Right type2 | type1 /= type2 -> ["Variable " ++ identifier ++ ": Type of expression different from type of variable."]
          Left err -> ["Variable " ++ identifier ++ ": " ++ err]
          _ -> []
  put ((identifier, VarType type1) : varContext)
  return (reservedError ++ duplicateError ++ typeError)

checkCommand :: Command -> State VarContext [Error]
checkCommand (Assignment var expr) = do
  checkAssignment var expr
checkCommand (PrintInt expr) = do
  checkPrintInt expr
checkCommand (GetInt var) = do
  checkGetInt var
checkCommand (If cond cmd1 cmd2) = do
  checkIf cond cmd1 cmd2
checkCommand (While cond cmd) = do
  checkWhile cond cmd
checkCommand (BeginEnd cmds) = concat <$> mapM checkCommand cmds

checkAssignment :: Identifier -> Expr -> State VarContext [Error]
checkAssignment var expr = do
  varContext <- get
  let assignmentError =
        case lookup var varContext of
          Just t -> case checkExpr varContext expr of
            Right t' | t /= VarType t' -> ["Assignment: Variable " ++ var ++ " expression has the wrong type"]
            Left err -> ["Assignment: Variable " ++ var ++ " " ++ err]
            _ -> []
          Nothing -> ["Assignment: Variable " ++ var ++ " not declared"]
  return assignmentError

checkPrintInt :: Expr -> State VarContext [Error]
checkPrintInt expr = do
  varContext <- get
  let printError =
        case checkExpr varContext expr of
          Right TypeInt -> []
          Right _ -> ["printint: Expression doesn't type-check"]
          Left err -> ["printint: " ++ err]
  return printError

checkGetInt :: Identifier -> State VarContext [Error]
checkGetInt var = do
  varContext <- get
  let getError =
        case lookup var varContext of
          Just (VarType TypeInt) -> []
          Just (VarType _) -> ["getint: Variable " ++ var ++ " is not an integer"]
          Nothing -> ["getint: Variable " ++ var ++ " not declared"]
  return getError

checkIf :: Expr -> Command -> Command -> State VarContext [Error]
checkIf cond cmd1 cmd2 = do
  varContext <- get
  let conditionError =
        case checkExpr varContext cond of
          Right TypeBool -> []
          Right _ -> ["If command: Expression is not Boolean"]
          Left err -> ["If command: " ++ err]
  errors1 <- checkCommand cmd1
  errors2 <- checkCommand cmd2
  return (conditionError ++ errors1 ++ errors2)

checkWhile :: Expr -> Command -> State VarContext [Error]
checkWhile cond cmd = do
  varContext <- get
  let conditionError =
        case checkExpr varContext cond of
          Right TypeBool -> []
          Right _ -> ["While condition: Must be Boolean"]
          Left err -> ["While condition: " ++ err]
  bodyErrors <- checkCommand cmd
  return (conditionError ++ bodyErrors)

checkExpr :: VarContext -> Expr -> Either Error Type
checkExpr varContext (LitInteger _) = Right TypeInt
checkExpr varContext (LitBoolean _) = Right TypeBool
checkExpr varContext (Var var) = checkVariable varContext var
checkExpr varContext (BinOp op expr1 expr2) = checkBinaryOp varContext op expr1 expr2
checkExpr varContext (UnOp op expr) = checkUnaryOp varContext op expr
checkExpr varContext (Conditional cond expr1 expr2) = checkConditional varContext cond expr1 expr2
checkExpr varContext (FunCall identifier args) = checkFunctionCall varContext identifier args

checkVariable :: VarContext -> Identifier -> Either Error Type
checkVariable varContext var =
  case find ((== var) . fst) varContext of
    Just (_, VarType type1) -> Right type1
    Just (_, FunType _ _) -> Left $ "Variable " ++ var ++ " is a function, not a variable."
    Nothing -> Left $ "Variable " ++ var ++ " not declared."

checkBinaryOp :: VarContext -> BinOperator -> Expr -> Expr -> Either Error Type
checkBinaryOp varContext op expr1 expr2 = do
  type1 <- checkExpr varContext expr1
  type2 <- checkExpr varContext expr2
  case (op, type1, type2) of
    (Addition, TypeInt, TypeInt) -> Right TypeInt
    (Subtraction, TypeInt, TypeInt) -> Right TypeInt
    (Multiplication, TypeInt, TypeInt) -> Right TypeInt
    (Division, TypeInt, TypeInt) -> Right TypeInt
    (Conjunction, TypeBool, TypeBool) -> Right TypeBool
    (Disjunction, TypeBool, TypeBool) -> Right TypeBool
    (LessThan, TypeInt, TypeInt) -> Right TypeBool
    (GreaterThan, TypeInt, TypeInt) -> Right TypeBool
    (Equal, t1, t2) | t1 == t2 -> Right TypeBool
    (NotEqual, t1, t2) | t1 == t2 -> Right TypeBool
    (LessThanOrEqual, TypeInt, TypeInt) -> Right TypeBool
    (GreaterThanOrEqual, TypeInt, TypeInt) -> Right TypeBool
    _ -> Left $ "Operator " ++ show op ++ " applied to incompatible types."

checkUnaryOp :: VarContext -> UnOperator -> Expr -> Either Error Type
checkUnaryOp varContext op expr = do
  type1 <- checkExpr varContext expr
  case (op, type1) of
    (Negation, TypeInt) -> Right TypeInt
    (Not, TypeBool) -> Right TypeBool
    _ -> Left $ "Unary operator " ++ show op ++ " applied to incompatible type."

checkConditional :: VarContext -> Expr -> Expr -> Expr -> Either Error Type
checkConditional varContext cond expr1 expr2 = do
  condType <- checkExpr varContext cond
  type1 <- checkExpr varContext expr1
  type2 <- checkExpr varContext expr2
  if condType == TypeBool && type1 == type2
    then Right type1
    else Left "Conditional expression has type errors."

checkFunctionCall :: VarContext -> Identifier -> [Expr] -> Either Error Type
checkFunctionCall varContext identifier args =
  case lookup identifier varContext of
    Just (FunType argTypes returnType) ->
      if length argTypes /= length args
        then Left $ "Function " ++ identifier ++ " called with incorrect number of arguments."
        else do
          -- Check types of arguments
          argChecked <- mapM (checkExpr varContext) args
          if argChecked == argTypes
            then Right returnType
            else Left $ "Function " ++ identifier ++ " called with incorrect argument types."
    Just (VarType _) -> Left $ "Identifier " ++ identifier ++ " is not a function."
    Nothing -> Left $ "Function " ++ identifier ++ " not declared."
