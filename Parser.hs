module Parser where

import Control.Applicative
import Grammar    
import Data.Char
import Debug.Trace
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

parseLeftAssoc :: Parser Expr -> Parser BinOperator -> Parser Expr
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
    operator <- (parseString "||") <|> (parseString "&&")
    return $ case operator of
        "||" -> Disjunction
        "&&" -> Conjunction

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

parseNegation :: Parser Expr
parseNegation = parseToken $ do
    satisfy (== '-')
    term <- parseTerm
    return (UnOp Negation term)

parseNot :: Parser Expr
parseNot = parseToken $ do
    satisfy (== '!')
    expr <- parseRelationExpr
    return (UnOp Not expr)

parseParentheses :: Parser Expr
parseParentheses = parseToken $ do
    satisfy (== '(')
    expr <- parseExpr
    satisfy (== ')')
    return expr

parseInteger :: Parser Expr
parseInteger = do
    int <- parseInt
    return (LitInteger int)

parseCondExpr :: Parser Expr
parseCondExpr = do
    b <- parseLogicExpr
    parseToken $ parseString "?"
    x <- parseLogicExpr
    parseToken $ parseString ":"
    y <- parseLogicExpr
    return (Conditional b x y)

parseExpr :: Parser Expr
parseExpr =   parseCondExpr <|> parseLogicExpr

parseLogicExpr :: Parser Expr
parseLogicExpr = parseLeftAssoc parseNotExpr parseBinOpBool

parseNotExpr :: Parser Expr
parseNotExpr = (parseNot <|> parseRelationExpr)

parseRelationExpr :: Parser Expr
parseRelationExpr = parseLeftAssoc parseAddSubExpr parseBinOpRel

parseAddSubExpr :: Parser Expr
parseAddSubExpr = parseLeftAssoc parseMulDivExpr parseBinOpAddSub

parseMulDivExpr :: Parser Expr
parseMulDivExpr = parseLeftAssoc parseTerm parseBinOpDivMul

parseTerm :: Parser Expr
parseTerm = 
    parseParentheses 
    <|> parseNegation
    <|> parseInteger

parseArith :: String -> Expr
parseArith input = case parse parseExpr input of
    [(ast, "")] -> ast
    _           -> error "invalid expression for parseArith"

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser (\input -> case input of
    (x:xs) | predicate x -> [(x, xs)]
    _                    -> [])

parseProgram :: Parser Program
parseProgram = do
    parseToken $ parseString "let"
    trace "Parsing declarations" (return ())
    decls <- parseDeclarations
    trace (show decls) (return ())
    parseToken $ parseString "in"
    command <- parseCommand
    return (LetIn decls command)

parseDeclaration :: Parser Declaration
parseDeclaration = parseVarAssign <|> parseVarDeclare

parseVarDeclare :: Parser Declaration
parseVarDeclare = do
    parseToken $ parseString "var"
    var <- parseIdentifier
    return (VarDeclare var)

parseVarAssign :: Parser Declaration
parseVarAssign = do
    parseToken $ parseString "var"
    var <- parseIdentifier
    parseToken $ parseString ":="
    expr <- parseExpr
    return (VarAssign var expr)

parseDeclarations :: Parser [Declaration]
parseDeclarations = do
    decl <- parseDeclaration
    decls <- many (parseToken $ parseString ";") *> parseDeclarations
    return (decl:decls)

parseCommand :: Parser Command
parseCommand = parseAssignment 
                <|> parseIfElse
                <|> parseWhile 
                <|> parseGetInt 
                <|> parsePrintInt 
                <|> parseBeginEnd

parseAssignment :: Parser Command
parseAssignment = do
    var <- parseIdentifier
    parseToken $ parseString ":="
    expr <- parseExpr
    return (Assignment var expr)

parseIfElse :: Parser Command
parseIfElse = do
    parseToken $ parseString "if"
    cond <- parseExpr
    parseToken $ parseString "then"
    cmd1 <- parseCommand
    parseToken $ parseString "else"
    cmd2 <- parseCommand
    return (If cond cmd1 cmd2)

parseWhile :: Parser Command
parseWhile = do
    parseToken $ parseString "while"
    cond <- parseExpr
    parseToken $ parseString "do"
    cmd <- parseCommand
    return (While cond cmd)

parseGetInt :: Parser Command
parseGetInt = do
    parseToken $ parseString "getint"
    var <- parseIdentifier
    return (GetInt var)

parsePrintInt :: Parser Command
parsePrintInt = do
    parseToken $ parseString "printint"
    expr <- parseExpr
    return (PrintInt expr)

parseBeginEnd :: Parser Command
parseBeginEnd = do
    parseToken $ parseString "begin"
    cmds <- parseCommands
    parseToken $ parseString "end"
    return (BeginEnd cmds)

parseCommands :: Parser [Command]
parseCommands = do
    cmd <- parseCommand
    cmds <- many (parseToken $ parseString ";") *> parseCommands
    return (cmd:cmds)

parseIdentifier :: Parser String
parseIdentifier = do
    c <- satisfy isAlpha
    cs <- many (satisfy isAlphaNum)
    return (c:cs)