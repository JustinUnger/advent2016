import Data.List

update :: Eq k => k -> [(k,Int)] -> [(k,Int)]
update k [] = [(k,1)]
update k ((k',v'):xs) = 
    if k == k' then (k,v' + 1) : xs else (k',v') : update k xs


foo [i0,i1,i2,i3,i4,i5,i6,i7] [h0,h1,h2,h3,h4,h5,h6,h7] =
    [update i0 h0, update i1 h1, update i2 h2, update i3 h3,
     update i4 h4, update i5 h5, update i6 h6, update i7 h7]

main = do
    s <- readFile "input.txt"
    let hs = foldr foo [[],[],[],[],[],[],[],[]] (lines s)
        sorted = map (reverse . sortOn snd) $ hs
    print $ map (fst . head) sorted
