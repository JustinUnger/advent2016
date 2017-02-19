module NanoParsec where

import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
    case parse m s of
        [(res, [])] -> res
        [(_, rs)]   -> error "Parser did not consume entire input"
        _           -> error "Parser error"

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s ->
        map (\(a,b) -> (f a, b)) (p s)

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Applicative Parser where
    pure = unit
    (Parser ab) <*> (Parser a) = Parser $ \s ->
        [(f x, s'') | (f, s') <- ab s, (x, s'') <- a s']

failure :: Parser a
failure = Parser (\_ -> [])

option :: Parser a -> Parser a -> Parser a
option (Parser f) (Parser g) = Parser $ \s ->
    case f s of
        [] -> g s
        r  -> r

instance Alternative Parser where
    empty = failure
    (<|>) = option

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser p) f = Parser $ \s -> concatMap (\(a,s') -> parse (f a) s') $ p s

instance Monad Parser where
    return = unit
    (>>=) = bind

item :: Parser Char
item = Parser $ \s ->
    case s of
    []     -> []
    (c:cs) -> [(c,cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- item
    if p c then return c else failure

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
    char c
    string cs
    return (c:cs)

isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']

digit :: Parser Char
digit = satisfy isDigit
    
natural :: Parser Int
natural = do
    cs <- some digit
    return $ read cs
