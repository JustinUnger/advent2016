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
  [1,2,3],
  [4,5,6],
  [7,8,9] 
 ]

key k (x,y) = k !! x !! y

type Position = (Int, Int)

data Direction =
    U | D | L | R deriving Show

move :: Direction -> Position -> Position
move U (0,y) = (0,y)
move U (x,y) = (x-1,y)
move D (2,y) = (2,y)
move D (x,y) = (x+1,y)
move L (x,0) = (x,0)
move L (x,y) = (x,y-1)
move R (x,2) = (x,2)
move R (x,y) = (x,y+1)

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
