data Heading =
    North |
    South | 
    East  |
    West 
    deriving Show

type Position = (Int, Int)

type MyState = (Heading, Position, [Position])

data Turn = L | R deriving Show
type Moves = Integer
type Insn = (Turn,Moves)

turn :: Turn -> Heading -> Heading
turn R North = West
turn R West  = South
turn R South = East
turn R East  = North

turn L North = East
turn L East  = South
turn L South = West
turn L West  = North

move :: Heading -> Position -> Position
move North (x,y) = (x, y+1)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)
move West  (x,y) = (x-1, y)

go :: Turn -> Moves -> Heading -> Position -> [Position] -> [Position]
go t n h p ps =
     let h' = turn t h 
         ps' = take (fromIntegral n) $ tail $ iterate (move h') p 
         in ps'

step :: Turn -> Moves -> MyState -> MyState
step t n (h, (x,y), ps) =
    let ps' = go t n h (last ps) ps in
    ((turn t h),(last ps'), ps ++ ps')

parse :: String -> Insn
parse ('L':xs) = (L,read xs)
parse ('R':xs) = (R,read xs)

myFold :: [Insn] -> MyState -> Maybe Position
myFold [] _ = Nothing
myFold (c:cs) (h, (x,y), ps) = 
   let (t,n) = c
       h'    = turn t h
       ps'   = go t n h (last ps) ps in
       case foo' ps' ps of
        Nothing -> myFold cs (h', (last ps'), (ps ++ ps'))    
        Just x  -> Just x

foo' :: Eq a => [a] -> [a] -> Maybe a
foo' [] _ = Nothing
foo' _ [] = Nothing
foo' (x:xs) ys = if elem x ys then Just x else foo' xs ys

distance :: Position -> Int
distance (x,y) = abs x + abs y

main :: IO ()
main = do
    s <- readFile "input.txt"
    let dirs = map parse (map (takeWhile (/= ',')) (words s))
    print dirs
    let firstDup = myFold dirs (North, (0,0), [(0,0)])
    print $ distance <$> firstDup

