type Triangle = (Int,Int,Int)

validTriangle :: Triangle -> Bool
validTriangle (a,b,c) = 
    (a + b > c) && (b + c > a) && (a + c > b) 

parse :: [String] -> [Triangle] 
parse [] = []
parse (x:y:z:zs) = [(read a,read b,read c),
                    (read a',read b',read c'),
                    (read a'',read b'', read c'')] ++ parse zs 
    where [a,a',a''] = words x
          [b,b',b''] = words y
          [c,c',c''] = words z

main = do
    s <- readFile "input.txt"
    let ts = parse (lines s)
    print $ length (filter validTriangle ts)

    
