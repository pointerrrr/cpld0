{-# OPTIONS -Wall #-}
module Infix where

import Data.Char

data Token = TNum Int | TPlus | TMul | TBOpen | TBClose
  deriving (Show, Eq)

run = eval . parserInfix . lexerInfix

prun = parserInfix . lexerInfix

lexerInfix :: String -> [Token]
lexerInfix [] = []
lexerInfix ('+':xs) = TPlus : lexerInfix xs
lexerInfix ('*':xs) = TMul : lexerInfix xs
lexerInfix ('(':xs) = TBOpen : lexerInfix xs
lexerInfix (')':xs) = TBClose : lexerInfix xs
lexerInfix xs = case span isDigit xs of ([], xs) -> case span isSpace xs of ([], xs) -> error "invalid character"
                                                                            (_, xs) -> lexerInfix xs
                                        (ds, xs) -> TNum (read ds) : lexerInfix xs

data SExpr = EAdd PExpr SExpr | SExpr PExpr
  deriving (Show)
  
data PExpr = EMul FExpr PExpr | PExpr FExpr
  deriving (Show)

data FExpr = EInt Int | FExpr SExpr
  deriving (Show)

parserInfix :: [Token] -> SExpr
parserInfix ts = case rem of [] -> case res of Just x -> x
                                               Nothing -> error "sad"
                             xs -> error (show xs)
  where
    (res, rem) = parseSExpr ts

type Parser r = [Token] -> (Maybe r, [Token])

parseSExpr :: Parser SExpr
parseSExpr ts = parseAdd ts </> (SExpr <$> pexpr, t1)
  where 
    (pexpr, t1) = parsePExpr ts

parseAdd :: Parser SExpr
parseAdd ts = (EAdd <$> s1 <* s2 <*> s3, t3)
  where
    (s1, t1) = parsePExpr ts
    (s2, t2) = parseToken TPlus t1
    (s3, t3) = parseSExpr t2

parsePExpr :: Parser PExpr
parsePExpr ts = parseMul ts </> (PExpr <$> fexpr, t1) 
  where
    (fexpr, t1) = parseFExpr ts

parseMul :: Parser PExpr
parseMul ts = (EMul <$> s1 <* s2 <*> s3, t3)
  where
    (s1, t1) = parseFExpr ts
    (s2, t2) = parseToken TMul t1
    (s3, t3) = parsePExpr t2

parseFExpr :: Parser FExpr
parseFExpr ts = parseInt ts </> (FExpr <$ s1 <*> s2 <* s3, t3 )
  where
    (s1, t1) = parseToken TBOpen ts
    (s2, t2) = parseSExpr t1
    (s3, t3) = parseToken TBClose t2

parseToken :: Token -> Parser ()
parseToken t1 (t2:ts) | t1 == t2 = (Just (), ts)
parseToken _ ts = (Nothing, ts)

parseInt :: Parser FExpr
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
 