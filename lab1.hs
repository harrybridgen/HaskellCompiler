import Data.Char 
import Control.Applicative
import Control.Monad

-- Grammar
-- expr  ::= mexpr | mexpr + expr | mexpr - expr
-- mexpr ::=  term | term * mexpr | term / mexpr
-- term  ::=   int |    -term     | ( expr )

-- Precendence
-- Brackets
-- Unary negation
-- Division
-- Multiplication
-- Subtraction
-- Addition

-- AST
-- 1+2*3
-- BinOp Addition (LitInteger 1) (BinOp Multiplication (LitInteger 2) (LitInteger 3))
--   +
--  / \
-- 1   *
--    /  \
--   2    3

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

-- Guard clause scanner
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

-- Define a parser for a natural number (sequence of digits)
parseInt :: Parser Integer
parseInt = do
    digits <- many (satisfy isDigit)
    return (read digits)

-- Define a parser for an operator
parseBinOp :: Parser BinOperator
parseBinOp = do
    operator <- satisfy (`elem` "+-*/")
    return $ case operator of
        '+' -> Addition
        '-' -> Subtraction
        '*' -> Multiplication
        '/' -> Division

-- make a parser that combines int and operator
parseIntOpInt :: Parser AST 
parseIntOpInt = do
    int1 <- parseInt
    op <- parseBinOp
    int2 <- parseInt
    return ( BinOp op (LitInteger int1) (LitInteger int2) )

parseTerm2 :: Parser AST
parseTerm2 = 
    do  satisfy (== '-')
        term <- parseTerm2
        return (UnOp Negation term)
    <|> 
    do  int1 <- parseInt
        return (LitInteger int1)

            
-- Helper function to parse based on a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\input -> case input of
    (x:xs) | predicate x -> [(x, xs)]
    _                    -> [])

monadEval :: String -> Integer
monadEval input = case parse parseTerm2 input of
    [(ast, "")] -> evaluate ast
    _           -> error "invalid expression"

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
