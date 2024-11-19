module Evaluator where

import Grammar
import Parser

evaluate :: Expr -> Integer
evaluate (LitInteger n) = n
evaluate (BinOp Addition a b) = evaluate a + evaluate b
evaluate (BinOp Subtraction a b) = evaluate a - evaluate b
evaluate (BinOp Multiplication a b) = evaluate a * evaluate b
evaluate (BinOp Division a b) = evaluate a `div` evaluate b
evaluate (UnOp Negation a) = -evaluate a
evaluate (BinOp Conjunction a b) = if evaluate a /= 0 && evaluate b /= 0 then 1 else 0
evaluate (BinOp Disjunction a b) = if evaluate a /= 0 || evaluate b /= 0 then 1 else 0
evaluate (UnOp Not a) = if evaluate a == 0 then 1 else 0
evaluate (BinOp LessThan a b) = if evaluate a < evaluate b then 1 else 0
evaluate (BinOp GreaterThan a b) = if evaluate a > evaluate b then 1 else 0
evaluate (BinOp Equal a b) = if evaluate a == evaluate b then 1 else 0
evaluate (BinOp LessThanOrEqual a b) = if evaluate a <= evaluate b then 1 else 0
evaluate (BinOp GreaterThanOrEqual a b) = if evaluate a >= evaluate b then 1 else 0
evaluate (BinOp NotEqual a b) = if evaluate a /= evaluate b then 1 else 0
evaluate (Conditional b x y) = if evaluate b /= 0 then evaluate x else evaluate y

eval :: String -> Integer
eval input = case parse parseExpr input of
  [(expr, "")] -> evaluate expr
  _ -> error "invalid expression for eval"
