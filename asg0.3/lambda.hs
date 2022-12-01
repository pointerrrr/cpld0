{-# OPTIONS -Wall #-}
module Lambda where

import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Show.Functions ()

data Token = TLambda | TApp | TVar String | TNum Int
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('#':xs) = TLambda : lexer xs
lexer ('`':xs) = TApp : lexer xs
lexer input@(x:xs)
  | isSpace x = lexer xs
  | otherwise = case span isDigit input of ([], xs) -> (\(var, xs) -> TVar var : lexer xs ) $ span notSpaceOrIdent xs
                                           (ds, xs) -> TNum (read ds) : lexer xs

notSpaceOrIdent :: Char -> Bool
notSpaceOrIdent '`' = False
notSpaceOrIdent '#' = False
notSpaceOrIdent c = not $ isSpace c

data Expr = ELambda String Expr | EApp Expr Expr | EVar String | ENum Int
  deriving (Show)

type Parser a = [Token] -> (Maybe a, [Token])

(</>) :: Parser Expr -> Parser Expr -> Parser Expr
(a </> b) ts = case a ts of (Just e, ts) -> (Just e, ts)
                            _ -> b ts

(<#>) :: (a -> b) -> Parser a -> Parser b
(a <#> b) ts = (\(x, ts) -> (a <$> x, ts) ) $ b ts

(<+>) :: Parser (a -> b) -> Parser a -> Parser b
(a <+> b) ts = case a ts of (x, ts) -> case b ts of (y, ts) -> (x <*> y, ts)

parser :: [Token] -> Expr
parser ts = case parser' ts of (Just e, []) -> e
                               (Nothing, ts) -> error $ "invalid parse " ++ show ts
                               (_, ts) -> error "incomplete parse"

parser' :: Parser Expr
parser' = parseLambda </> parseApp </> parseVar </> parseNum

parseLambda :: Parser Expr
parseLambda (TLambda:ts) = (ELambda <#> parseString <+> parser') ts
parseLambda ts = (Nothing, ts)

parseString :: Parser String
parseString (TVar s:ts) = (Just s, ts)
parseString ts = (Nothing, ts)

parseApp :: Parser Expr
parseApp (TApp:ts) = (EApp <#> parser' <+> parser') ts
parseApp ts = (Nothing, ts)

parseVar :: Parser Expr
parseVar = EVar <#> parseString

parseNum :: Parser Expr
parseNum (TNum i:ts) = (Just (ENum i), ts)
parseNum ts = (Nothing, ts)

data Value = VInt Int | VFun (Value -> Value)
  deriving (Show)

eval :: Expr -> Value
eval = eval' Map.empty

eval' :: Map String Value -> Expr -> Value
eval' m (ELambda s e) = VFun (\x -> eval' (Map.insert s x m) e)
eval' m (EApp e1 e2) = case eval' m e1 of VFun f -> f (eval' m e2)
                                          VInt i -> error "app on int"
eval' m (EVar s) = eLookup m s
eval' m (ENum i) = VInt i

eLookup :: Map String Value -> String -> Value
eLookup m "add" = VFun (\(VInt x) -> VFun (\(VInt y) -> VInt (x + y)))
eLookup m "mul" = VFun (\(VInt x) -> VFun (\(VInt y) -> VInt (x * y)))
eLookup m s = fromJust $ Map.lookup s m

interpret :: String -> Value
interpret = eval . parser . lexer

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _ = error "Nothing in fromJust"