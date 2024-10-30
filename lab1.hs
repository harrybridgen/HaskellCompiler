import Data.Char 
import Control.Applicative
import Control.Monad

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

-- Parser type 
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

-- Applies a parser to input string
parse :: Parser a -> String -> [(a, String)]
parse (Parser pFunct) input = pFunct input

-- Functor instance for Parser to allow for fmap usage
-- The functor (in this example) 
-- Takes in a function "fFunct" and a parser "pFunct"
-- We wrap the output within a parser since fmap returns Parser b
-- The parser takes an input, applies the parser "pFunct" to it returning (a,String)
-- We then apply the function "fFunct" to the result of the parser
instance Functor Parser where
    fmap fFunct (Parser pFunct) = Parser (\input -> [(fFunct result, rest) | (result, rest) <- pFunct input])

-- Applicative instance for Parser to allow for <*> usage
-- The applicative (in this example)
-- Takes in two parsers "pFunct1" and "pFunct2"
-- We then apply the first parser to the input returning (f, rest1)
-- We then apply the second parser to the remaining input returning (result, rest2)
-- We then apply the function "f" to the result of the first parser
instance Applicative Parser where
    pure result = Parser (\input -> [(result, input)])
    (Parser pFunct1) <*> (Parser pFunct2) = Parser (\input ->
        [(f result, rest2) | 
        (f, rest1) <- pFunct1 input, 
        (result, rest2) <- pFunct2 rest1])

-- Monad instance for Parser to allow for do notation
-- The result of the first parser is passed to the second parser
-- The second parser is then run on the remaining input
-- The results of both parsers are concatenated
instance Monad Parser where
    return = pure
    (Parser pFunct) >>= pFunct2 = Parser (\input->
        concat [runParser (pFunct2 result) rest | (result, rest) <- pFunct input])

-- Alternative instance for Parser to allow for <|> usage
-- This allows for the parser to try multiple parsers
instance Alternative Parser where
    empty = Parser (const [])
    (Parser pFunct1) <|> (Parser pFunct2) = Parser (\input ->
        let results = pFunct1 input in if null results then pFunct2 input else results)

-- Grammar
-- expr  ::= mexpr | mexpr + expr | mexpr - expr
-- mexpr ::=  term | term * mexpr | term / mexpr
-- term  ::=   int |    -term     | ( expr )

-- Precendence
-- Brackets
-- Unary negation
-- Division,Multiplication
-- Subtraction,Addition

-- AST
-- 1+2*3
-- BinOp Addition (LitInteger 1) (BinOp Multiplication (LitInteger 2) (LitInteger 3))
--   +
--  / \
-- 1   *
--    /  \
--   2    3

parseInt :: Parser Integer
parseInt = do
    digits <- many (satisfy isDigit)
    return (read digits)

parseBinOpDivMul :: Parser BinOperator
parseBinOpDivMul = do
    operator <- satisfy (`elem` "*/")
    return $ case operator of
        '*' -> Multiplication
        '/' -> Division

parseBinOpAddSub :: Parser BinOperator
parseBinOpAddSub = do
    operator <- satisfy (`elem` "+-")
    return $ case operator of
        '+' -> Addition
        '-' -> Subtraction

parseExpr :: Parser AST 
parseExpr = do
    mExpr <- parseMExpr
    op <- parseBinOpAddSub
    expr <- parseExpr
    return ( BinOp op (mExpr) (expr) )
    <|>
    do
    expr3 <- parseMExpr
    return expr3

parseMExpr :: Parser AST
parseMExpr = do
    term <- parseTerm
    op <- parseBinOpDivMul
    mExpr <- parseMExpr
    return ( BinOp op (term) (mExpr) )
    <|>
    do
    term <- parseTerm
    return term

parseTerm :: Parser AST
parseTerm = 
    do  satisfy (== '-')
        term <- parseTerm
        return (UnOp Negation term)
    <|> 
    do  satisfy (== '(')
        expr <- parseExpr
        satisfy (== ')')
        return expr
    <|>
    do  int1 <- parseInt
        return (LitInteger int1)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\input -> case input of
    (x:xs) | predicate x -> [(x, xs)]
    _                    -> [])

evaluate :: AST -> Integer
evaluate (LitInteger n) = n
evaluate (BinOp Addition a b) = evaluate a + evaluate b
evaluate (BinOp Subtraction a b) = evaluate a - evaluate b 
evaluate (BinOp Multiplication a b) = evaluate a * evaluate b 
evaluate (BinOp Division a b ) = evaluate a `div` evaluate b 
evaluate (UnOp Negation a) = - evaluate a


eval :: String -> Integer
eval input = case parse parseExpr input of
    [(ast, "")] -> evaluate ast
    _           -> error "invalid expression"

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
compArith expression = case parse parseExpr expression of
  [(ast,"")] -> expCode ast
  _ -> error "compArith error"
