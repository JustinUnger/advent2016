type Triangle = (Int,Int,Int)

validTriangle :: Triangle -> Bool
validTriangle (a,b,c) = 
    (a + b > c) && (b + c > a) && (a + c > b) 

parse :: [String] -> Triangle
parse [a,b,c] = (read a, read b, read c)

main = do
    s <- readFile "input.txt"
    let ts = map parse (map words (lines s))
    print $ length (filter validTriangle ts)
