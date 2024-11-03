module Evaluator (evaluate, eval) where

import Grammar
import Parser

evaluate :: Expr -> Integer
evaluate (LitInteger n) = n
evaluate (BinOp Addition a b) = evaluate a + evaluate b
evaluate (BinOp Subtraction a b) = evaluate a - evaluate b

eval :: String -> Integer
eval input = case parse parseExpr input of
  [(expr, "")] -> evaluate expr
  _ -> error "invalid expression for eval"
