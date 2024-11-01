import Data.Char 
import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf
import System.Environment
import System.FilePath (takeExtension, takeBaseName)


data AST = LitInteger Integer              -- For integer literals
         | BinOp BinOperator AST AST       -- For binary operations
         | UnOp UnOperator AST             -- For unary operations
         | Cond AST AST AST                -- For conditional operator
         deriving (Show)

data BinOperator = Addition | Subtraction | Multiplication | Division | Mod
                 | Conjunction | Disjunction
                 | LessThan | GreaterThan | Equal
                 | LessThanOrEqual | GreaterThanOrEqual | NotEqual
                 deriving (Show)

data UnOperator = Negation | Absolute | Not
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, "--trace"] | takeExtension file == ".tam" -> do
      contents <- readFile file
      let tamInstructions = read contents :: [TAMInst]
      finalStack <- traceTAM [] tamInstructions
      --putStrLn $ "Final stack: " ++ show finalStack
      printf "%-10s\t%s\n" ("Final stack: ") (show finalStack)

    [file, "--run"] | takeExtension file == ".exp" -> do
      contents <- readFile file
      let ast = parseArith contents
          tamInstructions = expCode ast
      putStrLn "Running generated TAM code:"
      print (execTAM [] tamInstructions)

    [file, "--evaluate"] | takeExtension file == ".exp" -> do
      contents <- readFile file
      let ast = parseArith contents
      print $ "Evaluation result: " ++ show (evaluate ast)

    [file] -> case takeExtension file of
      ".exp" -> do
        contents <- readFile file
        let ast = parseArith contents
        let tamFile = replaceExtension file ".tam"
        writeFile tamFile (show (expCode ast))
        putStrLn $ "Compiled to " ++ tamFile

      ".tam" -> do
        contents <- readFile file
        let tamInstructions = expCode (parseArith contents)
        print $ "Execution result: " ++ show (execTAM [] tamInstructions)

    _ -> putStrLn "Usage: program <file> [--trace | --run | --evaluate]"


replaceExtension :: FilePath -> String -> FilePath
replaceExtension file newExt = (takeBaseName file) ++ newExt

parseLeftAssoc :: Parser AST -> Parser BinOperator -> Parser AST
parseLeftAssoc termParser opParser = do
    initial <- termParser
    rest initial
  where
    rest left = (do
        op <- opParser
        right <- termParser
        rest (BinOp op left right)) <|> return left


parseSpace :: Parser ()
parseSpace = do
    _ <- many (satisfy isSpace)
    return ()

parseToken :: Parser a -> Parser a
parseToken p = do parseSpace
                  v <- p
                  parseSpace
                  return v

parseString :: String -> Parser String
parseString = traverse (satisfy . (==))

parseBinOpBool :: Parser BinOperator
parseBinOpBool = parseToken $ do
    operator <- (parseString "&&") <|> (parseString "||")
    return $ case operator of
        "&&" -> Conjunction
        "||" -> Disjunction

parseBinOpRel :: Parser BinOperator
parseBinOpRel = parseToken $ do
    operator <- (parseString "<=") <|> (parseString ">=") <|> (parseString "==") <|> (parseString "!=")
                <|> (parseString "<") <|> (parseString ">")
    return $ case operator of
        "<"  -> LessThan
        ">"  -> GreaterThan
        "==" -> Equal
        "<=" -> LessThanOrEqual
        ">=" -> GreaterThanOrEqual
        "!=" -> NotEqual

parseUnOpBool :: Parser UnOperator
parseUnOpBool = parseToken $ do
    satisfy (== '!')
    return Not

parseInt :: Parser Integer
parseInt = parseToken $ do
    digits <- many (satisfy isDigit)
    return (read digits)

parseBinOpAddSub :: Parser BinOperator
parseBinOpAddSub = parseToken $ do
    operator <- satisfy (`elem` "+-")
    return $ case operator of
        '+' -> Addition
        '-' -> Subtraction

parseBinOpDivMul :: Parser BinOperator
parseBinOpDivMul = parseToken $ do
    operator <- satisfy (`elem` "*/%")
    return $ case operator of
        '*' -> Multiplication
        '/' -> Division
        '%' -> Mod
parseNegation :: Parser AST
parseNegation = parseToken $ do
    satisfy (== '-')
    term <- parseTerm
    return (UnOp Negation term)

parseAbsolute :: Parser AST
parseAbsolute = parseToken $ do
    satisfy (== '|')
    expr <- parseExpr
    satisfy (== '|')
    return (UnOp Absolute expr)

parseNot :: Parser AST
parseNot = parseToken $ do
    satisfy (== '!')
    expr <- parseRelationExpr
    return (UnOp Not expr)

parseParentheses :: Parser AST
parseParentheses = parseToken $ do
    satisfy (== '(')
    expr <- parseExpr
    satisfy (== ')')
    return expr

parseInteger :: Parser AST
parseInteger = do
    int <- parseInt
    return (LitInteger int)

parseCondExpr :: Parser AST
parseCondExpr = do
    b <- parseLogicExpr
    parseToken $ parseString "?"
    x <- parseLogicExpr
    parseToken $ parseString ":"
    y <- parseLogicExpr
    return (Cond b x y)

parseExpr :: Parser AST
parseExpr =   parseCondExpr <|> parseLogicExpr

parseLogicExpr :: Parser AST
parseLogicExpr = parseLeftAssoc parseNotExpr parseBinOpBool

parseNotExpr :: Parser AST
parseNotExpr = (parseNot <|> parseRelationExpr)

parseRelationExpr :: Parser AST
parseRelationExpr = parseLeftAssoc parseAddSubExpr parseBinOpRel

parseAddSubExpr :: Parser AST
parseAddSubExpr = parseLeftAssoc parseMulDivExpr parseBinOpAddSub

parseMulDivExpr :: Parser AST
parseMulDivExpr = parseLeftAssoc parseTerm parseBinOpDivMul

parseTerm :: Parser AST
parseTerm = 
    parseAbsolute 
    <|> parseParentheses 
    <|> parseNegation
    <|> parseInteger


parseArith :: String -> AST
parseArith input = case parse parseExpr input of
    [(ast, "")] -> ast
    _           -> error "invalid expression for parseArith"

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\input -> case input of
    (x:xs) | predicate x -> [(x, xs)]
    _                    -> [])

evaluate :: AST -> Integer
evaluate (LitInteger n) = n
evaluate (BinOp Addition a b) = evaluate a + evaluate b
evaluate (BinOp Subtraction a b) = evaluate a - evaluate b 
evaluate (BinOp Multiplication a b) = evaluate a * evaluate b 
evaluate (BinOp Division a b) = evaluate a `div` evaluate b
evaluate (BinOp Mod a b) = evaluate a `mod` evaluate b
evaluate (UnOp Negation a) = - evaluate a
evaluate (UnOp Absolute a) = abs (evaluate a)
evaluate (BinOp Conjunction a b) = if evaluate a /= 0 && evaluate b /= 0 then 1 else 0
evaluate (BinOp Disjunction a b) = if evaluate a /= 0 || evaluate b /= 0 then 1 else 0
evaluate (UnOp Not a) = if evaluate a == 0 then 1 else 0
evaluate (BinOp LessThan a b) = if evaluate a < evaluate b then 1 else 0
evaluate (BinOp GreaterThan a b) = if evaluate a > evaluate b then 1 else 0
evaluate (BinOp Equal a b) = if evaluate a == evaluate b then 1 else 0
evaluate (BinOp LessThanOrEqual a b) = if evaluate a <= evaluate b then 1 else 0
evaluate (BinOp GreaterThanOrEqual a b) = if evaluate a >= evaluate b then 1 else 0
evaluate (BinOp NotEqual a b) = if evaluate a /= evaluate b then 1 else 0
evaluate (Cond b x y) = if evaluate b /= 0 then evaluate x else evaluate y

eval :: String -> Integer
eval input = case parse (parseExpr) input of
    [(ast, "")] -> evaluate ast
    _           -> error "invalid expression for eval"

type Stack = [Integer]

data TAMInst
  = LOADL Integer
  | ADD | SUB | MUL | DIV
  | NEG | MOD | ABS
  | AND | OR | NOT
  | LSS | GTR | EQL
  deriving (Read, Show)

execute :: Stack -> TAMInst -> Stack
execute stack (LOADL x) = x : stack
execute (y : x : rest) ADD = (x + y) : rest
execute (y : x : rest) SUB = (x - y) : rest
execute (y : x : rest) MUL = (x * y) : rest
execute (y : x : rest) DIV = (x `div` y) : rest
execute (y : x : rest) MOD = (x `mod` y) : rest
execute (x : rest) NEG = (-x : rest)
execute (x : rest) ABS = (abs x : rest)
execute (y : x : rest) AND = ((if x /= 0 && y /= 0 then 1 else 0) : rest)
execute (y : x : rest) OR = ((if x /= 0 || y /= 0 then 1 else 0) : rest)
execute (x : rest) NOT = ((if x == 0 then 1 else 0) : rest)
execute (y : x : rest) LSS = ((if x < y then 1 else 0) : rest)
execute (y : x : rest) GTR = ((if x > y then 1 else 0) : rest)
execute (y : x : rest) EQL = ((if x == y then 1 else 0) : rest)

execTAM :: Stack -> [TAMInst] -> Stack
execTAM stack [] = stack
execTAM stack (x:rest) = execTAM (execute stack x) rest

traceTAM :: Stack -> [TAMInst] -> IO Stack
traceTAM stack instructions = do 
  printf "%-10s\t%s\n" ("Initial stack: ") (show stack)
  --putStrLn ("Initial stack: " ++ show stack)
  traceExecTAM stack instructions

traceExecTAM :: Stack -> [TAMInst] -> IO Stack
traceExecTAM stack (instruction:rest) = do
  printf "%-10s\t%s\n" (show instruction) (show (execute stack instruction))
  --putStrLn (show instruction ++ "\t" ++ show (execute stack instruction))
  traceExecTAM (execute stack instruction) rest
traceExecTAM stack [] = return stack

expCode :: AST -> [TAMInst]
expCode (LitInteger x) = [LOADL x]
expCode (BinOp Addition ast ast') = expCode ast ++ expCode ast' ++ [ADD]
expCode (BinOp Subtraction ast ast') = expCode ast ++ expCode ast' ++ [SUB]
expCode (BinOp Multiplication ast ast') = expCode ast ++ expCode ast' ++ [MUL]
expCode (BinOp Division ast ast') = expCode ast ++ expCode ast' ++ [DIV]
expCode (BinOp Mod ast ast') = expCode ast ++ expCode ast' ++ [MOD]
expCode (BinOp Conjunction ast ast') = expCode ast ++ expCode ast' ++ [AND]
expCode (BinOp Disjunction ast ast') = expCode ast ++ expCode ast' ++ [OR]
expCode (UnOp Negation ast) = expCode ast ++ [NEG]
expCode (UnOp Absolute ast) = expCode ast ++ [ABS]
expCode (UnOp Not ast) = expCode ast ++ [NOT]
expCode (BinOp LessThan ast ast') = expCode ast ++ expCode ast' ++ [LSS]
expCode (BinOp GreaterThan ast ast') = expCode ast ++ expCode ast' ++ [GTR]
expCode (BinOp Equal ast ast') = expCode ast ++ expCode ast' ++ [EQL]
expCode (BinOp LessThanOrEqual ast1 ast2) = 
    expCode ast1 ++ expCode ast2 ++ [LSS] ++
    expCode ast1 ++ expCode ast2 ++ [EQL] ++
    [OR]
expCode (BinOp GreaterThanOrEqual ast1 ast2) = 
    expCode ast1 ++ expCode ast2 ++ [GTR] ++
    expCode ast1 ++ expCode ast2 ++ [EQL] ++
    [OR]
expCode (BinOp NotEqual ast1 ast2) = 
    expCode ast1 ++ expCode ast2 ++ [EQL] ++
    [NOT]
expCode (Cond b x y) = 
    expCode b ++                     
    expCode x ++                     
    [MUL] ++                         
    [LOADL 1] ++ expCode b ++        
    [SUB] ++                         
    expCode y ++                     
    [MUL] ++                         
    [ADD]

compArith :: String -> [TAMInst]
compArith expression = case parse parseExpr expression of
  [(ast,"")] -> expCode ast
  _ -> error "compArith error"