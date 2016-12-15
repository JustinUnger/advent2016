import System.Environment
import Control.Monad.State

data Heading =
    North |
    South | 
    East  |
    West 
    deriving Show

type Position = (Int, Int)
type Line = (Position,Position)

data Insn = L Int | R Int deriving Show

type MyState = (Heading, Position, [Position])

step :: Insn -> MyState -> MyState
step (L d) s@(h, (x,y), ps) = do 
    let (h',p) = case h of
                   North -> (West,(x-d,y)) 
                   West -> (South,(x,y-d))
                   South ->(East,(x+d,y))
                   East -> (North,(x,y+d))
    if (elem p ps) then s else (h', p, p : ps)
    
step (R d) s@(h, (x,y), ps) = do
    let (h', p) = case h of
                    North -> (East,(x+d,y))
                    East  -> (South,(x,y-d))
                    South -> (West,(x-d,y))
                    West  -> (North,(x,y+d))
    if (elem p ps) then (h, p, ps) else (h', p, p : ps)

parse :: String -> Insn
parse ('L':xs) = L (read xs)
parse ('R':xs) = R (read xs)

myFold :: [Insn] -> MyState
myFold = foldl (flip step) (North, (0,0), [(0,0)])

distance :: MyState -> Int
distance (_,(x,y),_) = x+y

main :: IO ()
main = do
    [f] <- getArgs
    s <- readFile f
    let dirs = map parse (map (takeWhile (/= ',')) (words s))
    print dirs
    print $ myFold dirs
    print $ distance (myFold dirs)


-- find if two lines intercept
-- lines are either veritical (x = 3) or horizontrol (slope 0, y = 3)

isVertical :: Line -> Bool
isVertical ((x,_),(x',_)) = x == x'

intersect :: Line -> Line -> Maybe Position
intersect l@((x,y),(x',y')) l'@((a,b),(a',b'))    
    | isVertical l /= isVertical l' = Just intercept
    | otherwise = Nothing
        where intercept = if (x == a) then (x,y) else (x',y) 

