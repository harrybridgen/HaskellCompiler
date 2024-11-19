-- expression grammar
-- ----------------------------------------------
-- expr  ::= mexpr | mexpr + expr | mexpr - expr
-- mexpr ::=  term | term * mexpr | term / mexpr
-- term  ::=   int |    -term     | ( expr )
-- ----------------------------------------------

-- miniTriangle grammar
-- ----------------------------------------------
-- program      ::= let declarations in command
-- declaration  ::= var identifier | var identifier = expression
-- declarations ::= declaration | declaration ; declarations
-- command      ::= indentifer := expression
--              | if expression then command else command
--              | while expression do command
--              | getint identifier
--              | printint expression
--              | begin commands end
-- commands     ::= command | command ; commands
-- ----------------------------------------------

module Grammar where

data Expr
  = LitInteger Integer
  | Var Identifier
  | BinOp BinOperator Expr Expr
  | UnOp UnOperator Expr
  | Conditional Expr Expr Expr
  deriving (Show)

data Command
  = Assignment Identifier Expr
  | If Expr Command Command
  | While Expr Command
  | GetInt Identifier
  | PrintInt Expr
  | BeginEnd [Command]
  deriving (Show)

type Identifier = String

data Declaration
  = VarDeclare Identifier
  | VarInitialize Identifier Expr
  deriving (Show)

data Program = LetIn [Declaration] Command
  deriving (Show)

data BinOperator
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | Conjunction
  | Disjunction
  | LessThan
  | GreaterThan
  | Equal
  | LessThanOrEqual
  | GreaterThanOrEqual
  | NotEqual
  deriving (Show)

data UnOperator = Negation | Not
  deriving (Show)
