import System.Environment
import Control.Monad.State

data Heading =
    North |
    South | 
    East  |
    West 
    deriving Show

type Position = (Int, Int)

data Insn = L Int | R Int deriving Show

type MyState = (Heading, Position)

step :: Insn -> MyState -> MyState
step (L d) (h, (x,y)) = 
    case h of
        North -> (West,(x-d,y)) 
        West -> (South,(x,y-d))
        South ->(East,(x+d,y))
        East -> (North,(x,y+d))

step (R d) (h, (x,y)) = 
    case h of
        North -> (East,(x+d,y))
        East  -> (South,(x,y-d))
        South -> (West,(x-d,y))
        West  -> (North,(x,y+d))

parse :: String -> Insn
parse ('L':xs) = L (read xs)
parse ('R':xs) = R (read xs)

myFold :: [Insn] -> MyState
myFold = foldl (flip step) (North, (0,0))

distance :: MyState -> Int
distance (_,(x,y)) = x+y

main :: IO ()
main = do
    [f] <- getArgs
    s <- readFile f
    let dirs = map parse (map (takeWhile (/= ',')) (words s))
    print dirs
    print $ myFold dirs
    print $ distance (myFold dirs)
