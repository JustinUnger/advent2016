rowOn :: Int -> [Bool] -> [Bool]
rowOn _ [] = []
rowOn 0 xs = xs
rowOn n (_:xs) = True : rowOn (n-1) xs 

rect :: Int -> Int -> [[Bool]] -> [[Bool]]
rect 0 _ xs = xs
rect _ 0 xs = xs
rect _ _ [] = []
rect c r (x:xs) = rowOn c x : rect c (r-1) xs

rotateRowRight :: Int -> Int -> [[Bool]] -> [[Bool]]
rotateRowRight _ 0 xs = xs 
rotateRowRight _ _ [] = []
rotateRowRight 0 k (r:rs) = rotateRight k r : rs
rotateRowRight n k (r:rs) = r : rotateRowRight (n-1) k rs

rotateRight :: Int -> [Bool] -> [Bool]
rotateRight n = last' . take (n+1) . iterate rot 

rot :: [Bool] -> [Bool]
rot xs = last' xs : chop xs

last' :: [a] -> a
last' [] = undefined
last' (x:[]) = x
last' (_:xs) = last xs

chop :: [a] -> [a]
chop [] = []
chop (_:[]) = []
chop (x:xs) = x : chop xs

col :: Int -> [[Bool]] -> [Bool]
col _ [] = []
col n (r:rs) = r !! n : col n rs 


rotateColDown :: Int -> Int -> [[Bool]] -> [[Bool]]
rotateColDown n k xs = replaceCol n (rotateRight k (col n xs)) xs

replaceCol :: Int -> [a] -> [[a]] -> [[a]]
replaceCol _ [] xs = xs
replaceCol _ _ []  = []
replaceCol k (x:xs) (r:rs) = replaceElem k x r : replaceCol k xs rs 

replaceElem :: Int -> a -> [a] -> [a]
replaceElem _ _ [] = []
replaceElem 0 x' (_:xs) = x' : xs
replaceElem n x' (x:xs) = x : replaceElem (n-1) x' xs

showRow :: [Bool] -> String
showRow = foldr (\x acc -> if x then '#' : acc  else '.' : acc) "\n"

showDisp :: [[Bool]] -> String
showDisp = foldr (\x acc -> showRow x ++ acc) "\n"

initDisp :: Int -> Int -> [[Bool]]
initDisp x y = take y (repeat (take x (repeat False)))

countRow :: [Bool] -> Int
countRow = foldr (\x acc -> if x then acc + 1 else acc) 0

count :: [[Bool]] -> Int
count [] = 0
count (x:xs) = countRow x + count xs

{-
s0 = initDisp 7 3
s1 = rect 3 2 s0
s2 = rotateColDown 1 1 s1
s3 = rotateRowRight 0 4 s2
s4 = rotateColDown 1 1 s3
-}

data Cmd = Rect Int Int   |
           RotCol Int Int |
           RotRow Int Int deriving Show

runCmd :: Cmd -> [[Bool]] -> [[Bool]]
runCmd (Rect x y) xs = rect x y xs
runCmd (RotCol x y) xs = rotateColDown x y xs
runCmd (RotRow x y) xs = rotateRowRight x y xs

parse :: [String] -> Cmd
parse ["rect", r] = let (x,y) = parseDim r in Rect x y
parse ["rotate", "row", r, "by", n] = 
    let x  = parseY r
        n' = read n
    in RotRow x n' 
parse ["rotate","column", c, "by", n] =
    let x = parseX c
        n' = read n
    in RotCol x n'

parseDim :: String -> (Int, Int)
parseDim xs = let x = takeWhile (/= 'x') xs
                  y = tail (dropWhile (/= 'x') xs) 
              in (read x, read y)

parseY :: String -> Int
parseY ('y':'=':xs) = read xs

parseX :: String -> Int
parseX ('x':'=':xs) = read xs

main :: IO ()
main = do
    s <- readFile "input.txt"
    let cmds = foldr (\x acc -> parse (words x) : acc) [] (lines s)
    let disp = foldl (\acc x -> runCmd x acc) (initDisp 50 6) cmds
    putStr $ showDisp disp
    print $ count disp
