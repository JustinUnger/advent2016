import Control.Monad.State 

sample = "abba[mnop]qrst"

aba :: String -> [String]
aba (x:y:z:zs) = undefined
aba _ = []

abba :: String -> Bool
abba (a:b:c:d:xs) 
    | a == b    = abba (b:c:d:xs)
    | otherwise = 
        if [a,b] == [d,c] then True else abba (b:c:d:xs)
abba _ = False

pass' (_, s) = abba s  

pass :: [(Bool,String)] -> Bool
pass xs = let sup = filter fst xs
              hyp = filter (not . fst) xs
              in not (any pass' hyp) && any pass' sup

data Parser a = Parser (String -> Maybe (a, String))
runParser (Parser f) s = f s

addr :: String -> State (Bool,[(Bool,String)],String) [(Bool,String)]
addr [] = do
    (b,acc,acc1) <- get
    if acc1 == [] then return acc else return ((b, acc1) : acc)
--      return $ (b, acc1) : acc
addr (x:xs) = do
    (b,acc,acc1) <- get
    case b of
        True -> do
            case x of
                ']' -> do
                    put $ (not b, (b,acc1) : acc, "")
                    addr xs
                otherwise -> do
                    put $ (b, acc, acc1 ++ [x])
                    addr xs
        False -> do
            case x of 
                '[' -> do
                    put $ (not b, (b,acc1) : acc, "")
                    addr xs
                otherwise -> do
                    put $ (b, acc, acc1 ++ [x])
                    addr xs

{-
addr = \(b,acc,(x:xs)) ->
    case b of
        False -> if x == '[' then (True, "", xs) else (b, x : acc, xs)
        True  -> if x == ']' then (False, "", xs) else (b, x : acc, xs) 
-}

char :: Char -> Parser Char
char c =
    Parser $ \s -> 
        case s of 
            (x:xs) -> if c == x
                then Just (c, xs)
                else Nothing
            _ -> Nothing

initState = (False, [], "")

main = do
    s <- readFile "input.txt"
    let xs = map (\x -> evalState (addr x) initState) (lines s)
    print $ foldr (\x acc -> if pass x then acc + 1 else acc) 0 xs
