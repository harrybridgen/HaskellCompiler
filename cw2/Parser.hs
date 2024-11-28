module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Grammar

newtype Parser a = Parser {runParser :: String -> [(a, String)]}

parse :: Parser a -> String -> [(a, String)]
parse (Parser pFunct) = pFunct

instance Functor Parser where
  fmap fFunct (Parser pFunct) = Parser (map (\(result, rest) -> (fFunct result, rest)) . pFunct)

instance Applicative Parser where
  pure result = Parser (\input -> [(result, input)])
  (Parser pFunct1) <*> (Parser pFunct2) =
    Parser
      ( concatMap
          ( \(f, rest1) ->
              map (\(result, rest2) -> (f result, rest2)) (pFunct2 rest1)
          )
          . pFunct1
      )

instance Monad Parser where
  return = pure
  (Parser pFunct) >>= pFunct2 =
    Parser
      ( concatMap (\(result, rest) -> runParser (pFunct2 result) rest) . pFunct
      )

instance Alternative Parser where
  empty = Parser (const [])
  (Parser pFunct1) <|> (Parser pFunct2) =
    Parser
      ( \input ->
          let results = pFunct1 input in if null results then pFunct2 input else results
      )

parseProgram :: Parser Program
parseProgram = do
  parseToken $ parseString "let"
  decls <- parseDeclarations <|> pure []
  parseToken $ parseString "in"
  command <- parseCommand <|> return (BeginEnd [])
  return (LetIn decls command)

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

parseDeclaration :: Parser Declaration
parseDeclaration = do
  parseVarInitialize <|> parseVarDeclare <|> parseFunDefine

parseFunDefine :: Parser Declaration
parseFunDefine = do
  parseToken $ parseString "fun"
  funName <- parseIdentifier
  parseToken $ parseString "("
  params <- parseParams
  parseToken $ parseString ")"
  parseToken $ parseString ":"
  returnType <- parseType
  parseToken $ parseString "="
  FunDefine funName params returnType <$> parseExpr

parseVarInitialize :: Parser Declaration
parseVarInitialize = do
  parseToken $ parseString "var"
  var <- parseIdentifier
  parseToken $ parseString ":"
  dataType <- parseType
  parseToken $ parseString ":="
  VarInitialize var dataType <$> parseExpr

parseVarDeclare :: Parser Declaration
parseVarDeclare = do
  parseToken $ parseString "var"
  var <- parseIdentifier
  parseToken $ parseString ":"
  VarDeclare var <$> parseType

parseParam :: Parser (Identifier, Type)
parseParam = do
  var <- parseIdentifier
  parseToken $ parseString ":"
  dataType <- parseType
  return (var, dataType)

parseParams :: Parser [(Identifier, Type)]
parseParams = do
  first <- parseParam
  rest <- many (parseToken $ parseString ",") *> parseParams <|> return []
  return (first : rest)

parseType :: Parser Type
parseType = do
  parseToken (parseString "Integer" >> return TypeInt)
    <|> parseToken (parseString "Boolean" >> return TypeBool)

parseAssignment :: Parser Command
parseAssignment = do
  var <- parseIdentifier
  parseToken $ parseString ":="
  Assignment var <$> parseExpr

parseIfElse :: Parser Command
parseIfElse = do
  parseToken $ parseString "if"
  cond <- parseExpr
  parseToken $ parseString "then"
  cmd1 <- parseCommand
  parseToken $ parseString "else"
  If cond cmd1 <$> parseCommand

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
  cmds <- parseCommands <|> return []
  parseToken $ parseString "end"
  return (BeginEnd cmds)

parseCommands :: Parser [Command]
parseCommands =
  do
    cmd <- parseCommand
    cmds <- (parseToken (parseString ";") *> parseCommands) <|> return []
    return (cmd : cmds)

parseIdentifier :: Parser Identifier
parseIdentifier = do
  first <- satisfy isAlpha
  rest <- many (satisfy isAlphaNum)
  return (first : rest)

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
    <|> parseBoolean
    <|> parseFunctionCall
    <|> parseVariable
    <|> parseInteger

parseFunctionCall :: Parser Expr
parseFunctionCall = do
  funName <- parseIdentifier
  parseToken $ parseString "("
  args <- parseExprs
  parseToken $ parseString ")"
  return (FunCall funName args)

parseExprs :: Parser [Expr]
parseExprs = do
  first <- parseExpr
  rest <- many (parseToken (parseString ",") *> parseExpr)
  return (first : rest)

parseBoolean :: Parser Expr
parseBoolean = do
  parseToken $ parseTrue <|> parseFalse

parseTrue :: Parser Expr
parseTrue = parseToken $ do
  parseString "true"
  return (LitBoolean True)

parseFalse :: Parser Expr
parseFalse = parseToken $ do
  parseString "false"
  return (LitBoolean False)

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

parseCondExpr :: Parser Expr
parseCondExpr = do
  b <- parseLogicExpr
  parseToken $ parseString "?"
  x <- parseLogicExpr
  parseToken $ parseString ":"
  Conditional b x <$> parseLogicExpr

parseBinOpBool :: Parser BinOperator
parseBinOpBool = parseToken $ do
  operator <- parseString "||" <|> parseString "&&"
  return $ case operator of
    "||" -> Disjunction
    "&&" -> Conjunction

parseNot :: Parser Expr
parseNot = parseToken $ do
  parseToken $ parseString "!"
  UnOp Not <$> parseRelationExpr

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

parseInt :: Parser Integer
parseInt =
  parseToken $ do
    digits <- many (satisfy isDigit)
    return (read digits)

parseNegation :: Parser Expr
parseNegation = parseToken $ do
  parseToken $ parseString "-"
  UnOp Negation <$> parseTerm

parseParentheses :: Parser Expr
parseParentheses = parseToken $ do
  parseToken $ parseString "("
  expr <- parseExpr
  parseToken $ parseString ")"
  return expr

parseInteger :: Parser Expr
parseInteger = do
  LitInteger <$> parseInt

parseVariable :: Parser Expr
parseVariable = parseToken $ do
  Var <$> parseIdentifier

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate =
  Parser
    ( \input -> case input of
        (x : xs) | predicate x -> [(x, xs)]
        _ -> []
    )

-- Functions for testing in GHCi
parseArith :: String -> Expr
parseArith arith = case parse parseExpr arith of
  [(exprAST, "")] -> exprAST
  _ -> error "invalid expression"