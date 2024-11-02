-- arith grammar
-- expr  ::= mexpr | mexpr + expr | mexpr - expr
-- mexpr ::=  term | term * mexpr | term / mexpr
-- term  ::=   int |    -term     | ( expr )

-- miniTriangle grammar
-- program ::= let declarations in command
-- declaration ::= var identifier | var identifier = expression
-- declarations ::= declaration | declaration ; declarations
-- command ::= indentifer := expression
--            | if expression then command else command
--            | while expression do command
--            | getint identifier
--            | printint expression
--            | begin commands end
-- commands ::= command | command ; commands

-- data Expr = LitInteger Integer
--             | var String
--             | BinOp BinOperator Expr Expr
--             | UnOp UnOperator Expr
--             | Conditional Expr Expr Expr
-- data Command = Assignment String Expr
--                 | if else then Expr Command Command
--                 | while do Expr Command
--                 | getInt String
--                 | printInt Expr
--                 | beginEnd Command
-- data Declaration = varDeclare String
--                 | varInt String Expr
-- data Program = Letin [Declaration] Command

module Grammar where

data Expr
  = LitInteger Integer -- Integer literals
  | Var Identifier -- Variables
  | BinOp BinOperator Expr Expr -- Binary operations (e.g., +, -, *, /)
  | UnOp UnOperator Expr -- Unary operations (e.g., negation, not)
  | Conditional Expr Expr Expr -- Conditional operator (b ? x : y)
  deriving (Show)

data Command
  = Assignment Identifier Expr -- Variable assignment
  | If Expr Command Command -- If-then-else command
  | While Expr Command -- While loop
  | GetInt Identifier -- Input command for integers
  | PrintInt Expr -- Output command for integers
  | BeginEnd [Command] -- Block of commands
  deriving (Show)

data Identifier = Identifier String
  deriving (Show, Eq)

data Declaration
  = VarDeclare Identifier -- Variable declaration (var identifier)
  | VarAssign Identifier Expr -- Variable declaration with initialization (var identifier := expr)
  deriving (Show)

data Program = LetIn [Declaration] Command
  deriving (Show)

data BinOperator
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | Mod
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
