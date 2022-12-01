{-# OPTIONS -Wall #-}
module RPN where

import Data.Char

data Token = TNum Int | TPlus | TMul
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('+':xs) = TPlus : lexer xs
lexer ('*':xs) = TMul : lexer xs
lexer xs = case span isDigit xs of ([], xs) -> case span isSpace xs of ([], xs) -> error "invalid character"
                                                                       (_, xs) -> lexer xs
                                   (ds, xs) -> TNum (read ds) : lexer xs

data Expr = EInt Int | EAdd Expr Expr | EMul Expr Expr

  deriving (Show)

parser :: [Token] -> Expr
parser xs = parser' [] xs

parser' :: [Expr] -> [Token] -> Expr
parser' [x] [] = x
parser' xs ((TNum i):ys) = parser' (EInt i:xs) ys
parser' (x1:x2:xs) (TPlus:ys) = parser' (EAdd x2 x1:xs) ys
parser' (x1:x2:xs) (TMul:ys) = parser' (EMul x2 x1:xs) ys
parser' _ _ = error "invalid parse"

eval :: Expr -> Int
eval (EInt i) = i
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EMul e1 e2) = eval e1 * eval e2

run = eval . parser . lexer