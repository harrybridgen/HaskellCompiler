module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Grammar

-- Parser data type
newtype Parser a = Parser {runParser :: String -> [(a, String)]}

-- Applies a parser to input string
parse :: Parser a -> String -> [(a, String)]
parse (Parser pFunct) input = pFunct input

-- Functor instance for Parser to allow for fmap usage
instance Functor Parser where
  fmap fFunct (Parser pFunct) = Parser (\input -> [(fFunct result, rest) | (result, rest) <- pFunct input])

-- Applicative instance for Parser to allow for <*> usage
instance Applicative Parser where
  pure result = Parser (\input -> [(result, input)])
  (Parser pFunct1) <*> (Parser pFunct2) =
    Parser
      ( \input ->
          [ (f result, rest2)
            | (f, rest1) <- pFunct1 input,
              (result, rest2) <- pFunct2 rest1
          ]
      )

-- Monad instance for Parser to allow for do notation
instance Monad Parser where
  return = pure
  (Parser pFunct) >>= pFunct2 =
    Parser
      ( \input ->
          concat [runParser (pFunct2 result) rest | (result, rest) <- pFunct input]
      )

-- Alternative instance for Parser to allow for <|> usage
instance Alternative Parser where
  empty = Parser (const [])
  (Parser pFunct1) <|> (Parser pFunct2) =
    Parser
      ( \input ->
          let results = pFunct1 input in if null results then pFunct2 input else results
      )

parseLeftAssoc :: Parser Expr -> Parser BinOperator -> Parser Expr
parseLeftAssoc termParser opParser = do
  initial <- termParser
  rest initial
  where
    rest left =
      ( do
          op <- opParser
          right <- termParser
          rest (BinOp op left right)
      )
        <|> return left

parseSpace :: Parser ()
parseSpace = do
  _ <- many (satisfy isSpace)
  return ()

parseToken :: Parser a -> Parser a
parseToken p = do
  parseSpace
  v <- p
  parseSpace
  return v

parseString :: String -> Parser String
parseString = traverse (satisfy . (==))

parseBinOpBool :: Parser BinOperator
parseBinOpBool = parseToken $ do
  operator <- parseString "||" <|> parseString "&&"
  return $ case operator of
    "||" -> Disjunction
    "&&" -> Conjunction

parseBinOpRel :: Parser BinOperator
parseBinOpRel = parseToken $ do
  operator <-
    parseString "<="
      <|> parseString ">="
      <|> parseString "=="
      <|> parseString "!="
      <|> parseString "<"
      <|> parseString ">"
  return $ case operator of
    "<" -> LessThan
    ">" -> GreaterThan
    "==" -> Equal
    "<=" -> LessThanOrEqual
    ">=" -> GreaterThanOrEqual
    "!=" -> NotEqual

parseUnOpBool :: Parser UnOperator
parseUnOpBool = parseToken $ do
  satisfy (== '!')
  return Not

parseInt :: Parser Integer
parseInt =
  parseToken $ do
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
parseExpr = parseCondExpr <|> parseLogicExpr

parseLogicExpr :: Parser Expr
parseLogicExpr = parseLeftAssoc parseNotExpr parseBinOpBool

parseNotExpr :: Parser Expr
parseNotExpr = parseNot <|> parseRelationExpr

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
    <|> parseVariable
    <|> parseInteger

parseVariable :: Parser Expr
parseVariable = do
  var <- parseIdentifier
  return (Var var)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate =
  Parser
    ( \input -> case input of
        (x : xs) | predicate x -> [(x, xs)]
        _ -> []
    )

parseArith :: String -> Expr
parseArith arith = case parse parseExpr arith of
  [(exprAST, "")] -> exprAST
  _ -> error "invalid expression"

parseSource :: String -> Program
parseSource source = case parse parseProgram source of
  [(programAST, "")] -> programAST
  _ -> error "invalid source"

parseProgram :: Parser Program
parseProgram = do
  parseToken $ parseString "let"
  decls <- parseDeclarations
  parseToken $ parseString "in"
  command <- parseCommand
  return (LetIn decls command)

parseDeclaration :: Parser Declaration
parseDeclaration = do
  parseVarInitialize <|> parseVarDeclare

parseVarDeclare :: Parser Declaration
parseVarDeclare = do
  parseToken $ parseString "var"
  var <- parseIdentifier
  return (VarDeclare var)

parseVarInitialize :: Parser Declaration
parseVarInitialize = do
  parseToken $ parseString "var"
  var <- parseIdentifier
  parseToken $ parseString ":="
  expr <- parseExpr
  return (VarInitialize var expr)

parseDeclarations :: Parser [Declaration]
parseDeclarations = do
  decl <- parseDeclaration
  decls <- many (parseToken $ parseString ";") *> parseDeclarations <|> return []
  return (decl : decls)

parseCommand :: Parser Command
parseCommand =
  do
    parseAssignment
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
  While cond <$> parseCommand

parseGetInt :: Parser Command
parseGetInt = do
  parseToken $ parseString "getint"
  bracket <- parseToken $ parseString "("
  var <- parseIdentifier
  bracket <- parseToken $ parseString ")"
  return (GetInt var)

parsePrintInt :: Parser Command
parsePrintInt = do
  parseToken $ parseString "printint"
  bracket <- parseToken $ parseString "("
  expr <- parseExpr
  bracket <- parseToken $ parseString ")"
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
  cmds <- many (parseToken $ parseString ";") *> parseCommands <|> return []
  return (cmd : cmds)

parseIdentifier :: Parser Identifier
parseIdentifier = do
  first <- satisfy isAlpha
  rest <- many (satisfy isAlphaNum)
  return (first : rest)