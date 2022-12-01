{-# OPTIONS -Wall #-}
module Infix where

import Data.Char

data Token = TNum Int | TPlus | TMul | TBOpen | TBClose
  deriving (Show, Eq)

run = eval . parser . lexer

prun = parser . lexer

lexer :: String -> [Token]
lexer [] = []
lexer ('+':xs) = TPlus : lexer xs
lexer ('*':xs) = TMul : lexer xs
lexer ('(':xs) = TBOpen : lexer xs
lexer (')':xs) = TBClose : lexer xs
lexer xs = case span isDigit xs of ([], xs) -> case span isSpace xs of ([], xs) -> error "invalid character"
                                                                       (_, xs) -> lexer xs
                                   (ds, xs) -> TNum (read ds) : lexer xs

data SExpr = EAdd PExpr SExpr | SExpr PExpr
  deriving (Show)
  
data PExpr = EMul FExpr PExpr | PExpr FExpr
  deriving (Show)

data FExpr = EInt Int | FExpr SExpr
  deriving (Show)

parser :: [Token] -> SExpr
parser ts = case rem of [] -> case res of Just x -> x
                                          Nothing -> error "sad"
                        xs -> error (show xs)
  where
    (res, rem) = parseSExpr ts


parseSExpr :: [Token] -> (Maybe SExpr, [Token])
parseSExpr ts = parseAdd ts </> (SExpr <$> pexpr, t1)
  where 
    (pexpr, t1) = parsePExpr ts

parseAdd :: [Token] -> (Maybe SExpr, [Token])
parseAdd ts = (EAdd <$> s1 <* s2 <*> s3, t3)
  where
    (s1, t1) = parsePExpr ts
    (s2, t2) = parseToken TPlus t1
    (s3, t3) = parseSExpr t2

parsePExpr :: [Token] -> (Maybe PExpr, [Token])
parsePExpr ts = parseMul ts </> (PExpr <$> fexpr, t1) 
  where
    (fexpr, t1) = parseFExpr ts

parseMul :: [Token] -> (Maybe PExpr, [Token])
parseMul ts = (EMul <$> s1 <* s2 <*> s3, t3)
  where
    (s1, t1) = parseFExpr ts
    (s2, t2) = parseToken TMul t1
    (s3, t3) = parsePExpr t2

parseFExpr :: [Token] -> (Maybe FExpr, [Token])
parseFExpr ts = parseInt ts </> (FExpr <$ s1 <*> s2 <* s3, t3 )
  where
    (s1, t1) = parseToken TBOpen ts
    (s2, t2) = parseSExpr t1
    (s3, t3) = parseToken TBClose t2

parseToken :: Token -> [Token] -> (Maybe (), [Token])
parseToken t1 (t2:ts) | t1 == t2 = (Just (), ts)
parseToken _ ts = (Nothing, ts)

parseInt :: [Token] -> (Maybe FExpr, [Token])
parseInt (TNum i:ts) = (Just (EInt i), ts)
parseInt ts = (Nothing, ts)

(</>) :: (Maybe a, [Token]) -> (Maybe a, [Token]) -> (Maybe a, [Token])
(</>) p@(Just x, ts) _ = p
(</>) _ p = p



eval :: SExpr -> Int
eval (EAdd e1 e2) = evalPExpr e1 + eval e2
eval (SExpr e) = evalPExpr e

evalPExpr :: PExpr -> Int
evalPExpr (EMul e1 e2) = evalFExpr e1 * evalPExpr e2
evalPExpr (PExpr e) = evalFExpr e

evalFExpr :: FExpr -> Int
evalFExpr (EInt i) = i
evalFExpr (FExpr e) = eval e
 