import NanoParsec2
import Control.Monad

(<|>) = option

expandCountAllR :: Parser Int
expandCountAllR = do
    xs <- manyv (parseExpand >>= expandCount)
    return $ sum xs

expandCount :: (Int, Int) -> Parser Int
expandCount (1,n) = item >> unit n
expandCount (m,n) = do
    s <- items m
    let l = runParser expandCountAllR s
    unit (l*n)

parseExpand :: Parser (Int,Int)
parseExpand = dim <|> unit (1,1)

expand :: (Int, Int) -> Parser String
expand (m,n) = concat <$> replicate n <$> items m

expandR :: (Int, Int) -> Parser String
expandR (1,n) = replicate n <$> item
expandR (m,n) = do
    s <- items m
    let s' = runParser expandAllR s
    return $ concat $ replicate n s'

doOne = parseExpand >>= expand

doOneR = parseExpand >>= expandR

doAll = concat <$> manyv doOne
expandAllR = concat <$> manyv doOneR

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

testPassed :: Parser Int -> Bool
testPassed p = all (\(i,o) -> runParser p i == o) tests

main :: IO ()
main = do
    print $ "Test (length <$> expandAllR): " ++ (show (testPassed (length <$> expandAllR)))
    print $ "Test expandCountAllR: " ++ (show (testPassed expandCountAllR))
    s <- readFile "input.txt"
    let f = runParser expandCountAllR
    print $ f (concat (lines s)) 

