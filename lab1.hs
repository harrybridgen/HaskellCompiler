import Data.Char (isDigit)

data Token = IntLiteral Integer  -- Numbers
           | Oper Operator       -- Operators (no arity distinction)
           | OpenPar             -- Opening Parenthesis
           | ClosedPar           -- Closing Parenthesis
           deriving (Show)       -- Derive Show to allow printing

data Operator = Plus | Minus | Times | Divide
              deriving (Show)    -- Derive Show to allow printing

data AST = LitInteger Integer              -- For integer literals
         | BinOp BinOperator AST AST       -- For binary operations
         | UnOp UnOperator AST             -- For unary operations
         deriving (Show)

data BinOperator = Addition | Subtraction | Multiplication | Division
                 deriving (Show)

data UnOperator = Negation
                deriving (Show)

scan :: String -> [Token]
scan [] = []
scan (x:xs)
  | x == '+'  = Oper Plus : scan xs
  | x == '-'  = Oper Minus : scan xs
  | x == '*'  = Oper Times : scan xs
  | x == '/'  = Oper Divide : scan xs
  | x == '('  = OpenPar : scan xs
  | x == ')'  = ClosedPar : scan xs
  | x == ' '  = scan xs
  | isDigit x = let number = takeWhile isDigit (x:xs)
                    rest = dropWhile isDigit (x:xs)
                    in IntLiteral (read number) : scan rest
  | otherwise = error "invalid character"

newtype Parser a = P (String -> [(a,String)])

parseExpr :: [Token] -> Maybe (AST, [Token])
parseExpr [] = Nothing
parseExpr xs = case parseMExpr xs of
  Nothing -> Nothing
  Just (ast, Oper Plus:rest) -> case parseExpr rest of
    Just (tree, tokens) -> Just (BinOp Addition ast tree, tokens)

  Just (ast, Oper Minus:rest) -> case parseExpr rest of
    Just (tree, tokens) -> Just (BinOp Subtraction ast tree, tokens)

  Just (ast, toks) -> Just (ast, toks)

parseMExpr :: [Token] -> Maybe (AST, [Token])
parseMExpr [] = Nothing
parseMExpr xs = case parseTerm xs of
  Nothing -> Nothing
  Just (ast, Oper Times:rest) -> case parseMExpr rest of
    Just (tree, tokens) -> Just (BinOp Multiplication ast tree, tokens)

  Just (ast, Oper Divide:rest) -> case parseMExpr rest of
    Just (tree, tokens) -> Just (BinOp Division ast tree, tokens)

  Just (ast, toks) -> Just (ast, toks)

parseTerm :: [Token] -> Maybe (AST, [Token])
parseTerm [] = Nothing
parseTerm (x:xs) = case x of 
  IntLiteral n -> Just (LitInteger n, xs)
  Oper Minus -> case parseTerm xs of     
    Nothing -> Nothing
    Just (ast, tokens) -> Just (UnOp Negation ast, tokens) 
  OpenPar -> case parseExpr xs of
    Just (ast, ClosedPar:rest) -> Just (ast, rest)
    _ -> Nothing


evaluate :: AST -> Integer
evaluate (LitInteger n) = n
evaluate (BinOp Addition a b) = evaluate a + evaluate b
evaluate (BinOp Subtraction a b) = evaluate a - evaluate b 
evaluate (BinOp Multiplication a b) = evaluate a * evaluate b 
evaluate (BinOp Division a b ) = evaluate a `div` evaluate b 
evaluate (UnOp Negation a) = - evaluate a

eval :: String -> Integer
eval input = case parseExpr (scan input) of
  Just (ast, []) -> evaluate ast
  _ -> error "invalid expression"

type Stack = [Integer]

data TAMInst
  = LOADL Integer -- push Integer into the stack
  | ADD -- adds two top values in the stack
  | SUB -- subtract second element of stack from top
  | MUL -- multiplies top values in the stack
  | DIV -- divides the second value by the top
  | NEG -- negates the top of the stack
  deriving (Show)

execute :: Stack -> TAMInst -> Stack
execute stack (LOADL x) = x:stack
execute (y:x:rest) ADD = (x + y):rest
execute (y:x:rest) SUB = (x - y):rest
execute (y:x:rest) MUL = (x * y):rest
execute (y:x:rest) DIV = (x `div` y):rest
execute (x:rest) NEG = (-x:rest)

execTAM :: Stack -> [TAMInst] -> Stack
execTAM stack [] = stack
execTAM stack (x:rest) = execTAM (execute stack x) rest

traceTAM :: Stack -> [TAMInst] -> IO Stack
traceTAM stack instructions = do 
  putStrLn ("Initial stack: " ++ "\t" ++ show stack)
  traceExecTAM stack instructions

traceExecTAM :: Stack -> [TAMInst] -> IO Stack
traceExecTAM stack (instruction:rest) = do
  putStrLn ((show instruction) ++ "\t\t" ++ (show (execute stack instruction)))
  traceExecTAM (execute stack instruction) rest
traceExecTAM stack [] = return stack

expCode :: AST -> [TAMInst]
expCode (LitInteger x) = [LOADL x]
expCode (BinOp Addition ast ast') = (expCode ast) ++ (expCode ast') ++ [ADD]
expCode (BinOp Subtraction ast ast') = (expCode ast) ++ (expCode ast') ++ [SUB]
expCode (BinOp Multiplication ast ast') = (expCode ast) ++ (expCode ast') ++ [MUL]
expCode (BinOp Division ast ast') = (expCode ast) ++ (expCode ast') ++ [DIV]
expCode (UnOp Negation ast) = (expCode ast) ++ [NEG]

compArith :: String -> [TAMInst]
compArith expression = case parseExpr (scan expression) of
  Just (tree,[]) -> expCode tree
  _ -> error "compArith error"

{-
3 + 2 * (4 - 1)"
-> Scanner ->
[IntLiteral 3,Oper Plus,IntLiteral 2,Oper Times,OpenPar,IntLiteral 4,Oper Minus,IntLiteral 1,ClosedPar]
-> Parser ->
(BinOp Addition (LitInteger 3) (BinOp Multiplication (LitInteger 2) (BinOp Subtraction (LitInteger 4) (LitInteger 1)))
        +
       / \
      3   *
         / \
        2   -
           / \
          4   1 
-> Evaluator ->
4 - 1 = 3
2 * 3 = 6
3 + 6 = 9
-}

