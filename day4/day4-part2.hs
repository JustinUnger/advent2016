import Data.List
import Data.Char
import Data.Maybe

myBreak :: String -> [String]
myBreak [] = []
myBreak xs = x : myBreak (drop 1 rest)
    where (x,rest) = break (== '-') xs

update :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
update k [] = [(k,1)]
update k (x:xs) = 
    let (a,b) = x in if a == k then (k,b+1) : xs else x : update k xs

hist :: Eq a => [a] -> [(a,Int)]
hist = foldr update [] 

-- custom qsort: sort on value, then use alphabetic ordering for tie breaking

myQsort :: (Ord k, Ord v) => [(k,v)] -> [(k,v)]
myQsort [] = []
myQsort ((k,v):xs) = myQsort smaller ++ [(k,v)] ++ myQsort larger
    where smaller = filter (\(x,y) -> if y == v then x > k else y < v) xs
          larger  = filter (\(x,y) -> if y == v then x < k else y > v) xs

-- weird checksum: transform input string into histogram sorted highest frequency first with ties broken alphabetically

cksum' :: String -> String
cksum' = map fst . reverse . myQsort . hist . filter (/= '-')

cksum :: String -> String
cksum = take 5 . cksum'

{-
*Main> :t cksum
cksum :: [Char] -> [Char]
*Main> cksum "totally-real-room"
"loartemy"
*Main> cksum "aaaaa-bbb-z-y-x"
"abxyz"
*Main> cksum "aczupnetwp-mfyyj-opalcexpye"
"peyacfjlmnotuwxz"
*Main> 
-}

sampleRoom = "dzczkrip-xiruv-srjbvk-rercpjzj-607[rjzci]"

xs = myBreak sampleRoom

dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs


sectorId :: String -> Int
sectorId = read . takeWhile (/= '[') . last . myBreak

roomCksum :: String -> String
roomCksum = takeWhile (/= ']') . drop 1 . dropWhile (/= '[') . last . myBreak

roomName :: String -> String
roomName = intercalate "-" . dropLast . myBreak

testRoom' :: String -> Bool
testRoom' s = ((cksum . roomName) s) == (roomCksum s)

-- ooh fancy, use reader monad! is this easier to read? 

testRoom :: String -> Bool
testRoom = (==) <$> (cksum . roomName) <*> roomCksum

realRoomSectorSum :: [String] -> Int
realRoomSectorSum = foldr (\x acc -> if testRoom x then acc + (sectorId x) else acc) 0

rotateLetter ::  Int -> Char -> Char
rotateLetter _ '-' = ' '
rotateLetter x c = chr $ ((letter+x) `mod` 26) + ord 'a' 
    where letter = (ord c) - (ord 'a')

decryptRoomName :: String -> Maybe (Int,String)
decryptRoomName r = 
    if testRoom r then Just (sectorId r,n) else Nothing
    where n = map (rotateLetter (sectorId r)) (roomName r)

isMaybe :: Maybe a -> Bool
isMaybe Nothing = False
isMaybe (Just _) = True

main = do
    s <- readFile "input.txt"
    print $ filter (isPrefixOf "north" . snd) $ catMaybes $ map decryptRoomName (lines s) 
