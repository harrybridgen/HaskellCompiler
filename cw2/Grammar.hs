-- Grammar
-- ----------------------------------------------
-- expr        ::= term | term + expr | term - expr
-- mExpr       ::= expr | expr * mExpr | expr / mExpr
-- term        ::= int | bool | identifier | -term | (expr) | identifier (exprs)
-- exprs       ::= expr | expr , exprs
-- ----------------------------------------------
-- program     ::= let declarations in command
-- declaration ::= var identifier : type
--               | var identifier : type := expr
--               | fun identifier (vardecls) : type := expr
-- vardecl     ::= identifier : type
-- vardecls    ::= vardecl | vardecl , vardecls
-- type        ::= Integer | Boolean
-- ----------------------------------------------
-- command     ::= identifier := expr
--               | if expr then command else command
--               | while expr do command
--               | getint identifier
--               | printint expr
--               | begin commands end
-- commands    ::= command | command ; commands
-- ----------------------------------------------

module Grammar where

data Expr
  = LitInteger Integer
  | LitBoolean Bool
  | Var Identifier
  | BinOp BinOperator Expr Expr
  | UnOp UnOperator Expr
  | Conditional Expr Expr Expr
  | FunCall Identifier [Expr]
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

data Type = TypeInt | TypeBool
  deriving (Eq, Show)

data Declaration
  = VarDeclare Identifier Type
  | VarInitialize Identifier Type Expr
  | FunDefine Identifier [(Identifier, Type)] Type Expr
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
