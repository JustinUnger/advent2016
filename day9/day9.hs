import NanoParsec
import Control.Monad

dim :: Parser (Integer, Integer)
dim = do 
    char '('
    x <- natural
    char 'x'
    y <- natural
    char ')'
    return (x,y)

foo = bind dim (\(m,n) -> concat . (replicate (fromIntegral n)) <$> replicateM (fromIntegral m) item)

p :: Parser String
p = option (fmap (\x -> x : []) (nchar '(')) foo

myParser :: Parser [String]
myParser = many' p

nchar :: Char -> Parser Char
nchar c = satisfy (c /=)

main :: IO ()
main = do
    s <- readFile "input.txt"
    let f = concat . runParser myParser
    print $ length (f s)
