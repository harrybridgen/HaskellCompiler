module Compiler where

import Grammar
import Parser
import Text.Printf -- !! MUST REMEMBER TO REMOVE THIS !!

type Stack = [Integer]

data TAMInst
  = LOADL Integer
  | ADD | SUB | MUL | DIV
  | NEG | MOD
  | AND | OR | NOT
  | LSS | GTR | EQL
  | HALT | GETINT | PUTINT
  | LABEL Integer | JUMP Integer | JUMPIFZ Integer
  | LOAD Integer | STORE Integer
  deriving (Read, Show)

execute :: Stack -> TAMInst -> Stack
execute stack (LOADL x) = x : stack
execute (y : x : rest) ADD = (x + y) : rest
execute (y : x : rest) SUB = (x - y) : rest
execute (y : x : rest) MUL = (x * y) : rest
execute (y : x : rest) DIV = (x `div` y) : rest
execute (y : x : rest) MOD = (x `mod` y) : rest
execute (x : rest) NEG = (-x : rest)
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

expCode :: Expr -> [TAMInst]
expCode (LitInteger x) = [LOADL x]
expCode (BinOp Addition ast ast') = expCode ast ++ expCode ast' ++ [ADD]
expCode (BinOp Subtraction ast ast') = expCode ast ++ expCode ast' ++ [SUB]
expCode (BinOp Multiplication ast ast') = expCode ast ++ expCode ast' ++ [MUL]
expCode (BinOp Division ast ast') = expCode ast ++ expCode ast' ++ [DIV]
expCode (BinOp Mod ast ast') = expCode ast ++ expCode ast' ++ [MOD]
expCode (BinOp Conjunction ast ast') = expCode ast ++ expCode ast' ++ [AND]
expCode (BinOp Disjunction ast ast') = expCode ast ++ expCode ast' ++ [OR]
expCode (UnOp Negation ast) = expCode ast ++ [NEG]
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
expCode (Conditional b x y) = 
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

stringTraceTAM :: String -> IO Stack
stringTraceTAM expression = do
  let instructions = compArith expression
  traceTAM [] instructions