{- 

keypad

1 2 3 
4 5 6
7 8 9

moves up, down, left, right

start on 5

-}

keypad = 
 [
  "00100",
  "02340",
  "56789",
  "0ABC0",
  "00D00"
 ]

key k (x,y) = k !! x !! y

type Position = (Int, Int)

data Direction =
    U | D | L | R deriving Show

move :: Direction -> Position -> Position
move U (x,y)
    | x == 0 = (x,y)
    | key keypad (x-1,y) == '0' = (x,y)
    | otherwise = (x-1,y)
move D (x,y)
    | x == 4 = (x,y)
    | key keypad (x+1,y) == '0' = (x,y)
    | otherwise = (x+1,y) 
move L (x,y)    
    | y == 0 = (x,y)
    | key keypad (x,y-1) == '0' = (x,y)
    | otherwise = (x,y-1)
move R (x,y)
    | y == 4 = (x,y)
    | key keypad (x,y+1) == '0' = (x,y)
    | otherwise = (x,y+1)

parse :: Char -> Direction
parse 'U' = U
parse 'D' = D 
parse 'L' = L
parse 'R' = R


main = do
    s <- readFile "input.txt"
    let moves = (map . map) parse $ lines s    
        ps    = map (foldl (flip move) (1,1)) moves
    print $ map (key keypad) ps
