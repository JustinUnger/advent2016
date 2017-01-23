import NanoParsec2
import Control.Monad

(<|>) = option

foo :: Parser Int
foo = do
    xs <- manyv (dim <|> unit (1,1) >>= expandCount) 
    return $ sum xs

expandCount :: (Int, Int) -> Parser Int
expandCount (1,n) = item >> unit n
expandCount (m,n) = do
    s <- items m
    let x = runParser foo s
    unit (x*n)

parseExpand :: Parser (Int,Int)
parseExpand = dim <|> unit (1,1)

p1 :: Parser (Int,Int)
p1 = option dim (unit (1,1))

p2 :: (Int, Int) -> Parser String
p2 (m,n) = concat <$> replicate n <$> items m

expand :: (Int, Int) -> Parser String
expand (m,n) = concat <$> replicate n <$> items m

p2' :: (Int, Int) -> Parser String
p2' (1,n) = replicate n <$> item
p2' (m,n) = do
    s <- items m
    let s' = runParser p4' s
    return $ concat $ replicate n s'

expandR :: (Int, Int) -> Parser String
expandR (1,n) = replicate n <$> item
expandR (m,n) = do
    s <- items m
    let s' = runParser doAll s
    return $ concat $ replicate n s'

p2'' :: (Int, Int) -> Parser Int
p2'' (1,n) = item >> unit n
p2'' (m,n) = do
    s <- items m
    let x = runParser p4'' s
    unit (x*n)

p3 :: Parser String
p3 = p1 >>= p2

p3' :: Parser String
p3' = p1 >>= p2'

doOne = parseExpand >>= expandR

p3'' :: Parser Int
p3'' = p1 >>= p2''

p4' :: Parser String
p4' = concat <$> manyv p3'

doAll = concat <$> manyv doOne

p4'' :: Parser Int
p4'' = do
    xs <- manyv p3''
    unit $ foldr (\x acc -> x + acc) 0 xs

p4 :: Parser String
p4 = concat <$> manyv p3

dim :: Parser (Int, Int)
dim = do 
    char '('
    x <- natural
    char 'x'
    y <- natural
    char ')'
    return (fromIntegral x,fromIntegral y)

tests :: [(String, Int)]
tests = [
     ("(3x3)XYZ",9)
   , ("X(8x2)(3x3)ABCY", 20)
   , ("(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920)
   , ("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445)
   ]

--testPassed :: Boolean
testPassed p = map (\(i,o) -> runParser p i == o) tests 

main :: IO ()
main = do
    s <- readFile "input.txt"
    let f = runParser p4''
    -- subtract one for the newline in input
    print $ (f s) - 1

