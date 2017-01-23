module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
    case parse m s of
        [(res, [])] -> res
        [(_, rs)]   -> error "Parser did not consume entire stream."
        _           -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
    case s of
    []     -> []
    (c:cs) -> [(c,cs)]

{-
bar :: (a -> Parser b) -> (a, String) -> [(b, String)]
bar f (a,s) = parse (f a) s
foo f p s = concatMap (bar f) (parse p s)
-}

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Functor Parser where
    --fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])
    fmap f (Parser cs) = Parser $ \s ->
        map (\(a,b) -> (f a, b)) (cs s) 

instance Applicative Parser where
    pure = unit
    (Parser cs1) <*> (Parser cs2) = Parser $ \s ->
        [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

instance Alternative Parser where
    empty = failure
    (<|>) = option

instance Monad Parser where
    return = unit
    (>>=) = bind

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
        []  -> parse q s
        res -> res

failure :: Parser a
failure = Parser (\cs -> [])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- zero or more
many' :: Parser a -> Parser [a]
many' = manyv 

some' :: Parser a -> Parser [a]
some' = somev 
    
manyv :: Parser a -> Parser [a]
--manyv v = somev v <|> pure []
manyv v = option (somev v) (pure [])

somev :: Parser a -> Parser [a]
somev v = (:) <$> v <*> manyv v

instance Show (a -> b) where
    show _ = "<function>"

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c -> 
    if p c then unit c else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = unit []
string (c:cs) = do
    char c
    string cs
    return (c:cs)

string' :: String -> Parser String
string' [] = unit []
string' (c:cs) =
    -- char c >> string cs >> return (c:cs)
    char c >>= (\_ -> string' cs >>= (\_ -> return (c:cs)))

parens :: Parser a -> Parser a
parens m = do
    char '('
    n <- m
    char ')'
    return n

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
    where rest a = (do f <- op
                       b <- p
                       rest (a `f` b))
                    <|> return a

rest :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
rest p op a = (do f <- op; b <- p; rest p op (f a b)) <|> return a

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

spaces :: Parser String
spaces = many $ oneOf " \n\r"

reserved :: String -> Parser String
reserved s = token (string s)
