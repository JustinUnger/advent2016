import Control.Monad.State 

sample1 = "aba[bab]xyz" -- pass
sample2 = "xyx[xyx]xyx" -- fail
sample3 = "aaa[kek]eke" -- pass
sample4 = "zazbz[bzb]cdb" -- pass

ps1 = evalState (addr sample1) initState
ps2 = evalState (addr sample2) initState
ps3 = evalState (addr sample3) initState
ps4 = evalState (addr sample4) initState

aba :: String -> [String]
aba (x:y:z:zs) =
    if x /= y && x == z then [x,y,z] : (aba (y:z:zs)) else aba (y:z:zs)
aba _ = []

bab :: String -> String -> Bool
bab [a,b,c] (x:y:z:zs) = (a == x && b == y && c == z) || bab [a,b,c] (y:z:zs)
bab _ _ = False

pass :: [(Bool,String)] -> Bool
pass xs = let (_,hyp) = unzip (filter fst xs)
              (_,sup) = unzip (filter (not . fst) xs)
              f [a,b,_] = [b,a,b]
              abas = concatMap aba sup
              fs = bab . f <$> abas
              in myAny $ fs <*> hyp

myAny :: [Bool] -> Bool
myAny [] = False
myAny (x:xs) = if x then True else myAny xs

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
