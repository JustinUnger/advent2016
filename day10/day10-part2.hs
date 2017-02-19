import NanoParsec (runParser)
import Parser (Command(..),commands, OutputNum, BotNum, Value, Program(..), Action, Destination(..))

insert :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
insert k v [] = [(k,v)]
insert k v (kv:kvs) = if k == fst kv then (k,v) : kvs else kv : insert k v kvs

lookup' :: Eq k => k -> [(k,v)] -> Maybe v
lookup' _ [] = Nothing
lookup' k (x:xs) = let (k',v) = x in if k' == k then Just v else lookup k xs

type OutputMap = [(OutputNum,Value)]
type BotMap = [(BotNum,Bot)]
type World = (BotMap,OutputMap)

type Match = (OutputNum, OutputNum, OutputNum)

data Bot = Bot { 
      botNum :: BotNum
    , valueA :: Maybe Value
    , valueB :: Maybe Value
   , program :: Maybe Program } deriving Show

type Result = (Action,Action)

giveProgram' :: Bot -> Program -> Maybe Bot
giveProgram' b@(Bot { program = Nothing }) p = 
    Just b { program = Just p }
giveProgram' _ _ = Nothing

giveProgram :: BotNum -> Program -> BotMap -> Maybe Bot
giveProgram bn p bmap = case lookup' bn bmap of
    Nothing -> giveProgram' (createBot bn) p 
    Just b  -> giveProgram' b p

giveValue' :: Bot -> Value -> Maybe Bot
giveValue' b@(Bot { valueA = Nothing, valueB = Nothing }) v =
    Just b { valueA = Just v }
giveValue' b@(Bot { valueA = Just _, valueB = Nothing }) v =
    Just b { valueB = Just v }
giveValue' _ _ = Nothing

giveValue :: BotNum -> Value -> BotMap -> Maybe Bot
giveValue bn v bmap = case lookup' bn bmap of
    Nothing -> giveValue' (createBot bn) v
    Just b  -> giveValue' b v


run :: Match -> [Command] -> World -> Either Value World
run m [] w  = Right w 
run m (c:cs) w = case step m c w of
    Nothing -> error $ "command error: " ++ show c
    Just (Left v) -> Left v
    Just (Right w') -> run m cs w'

step :: Match -> Command -> World -> Maybe (Either Value World)
step m (GiveValue v bnum) w@(bmap,omap) = do
    bot <- giveValue bnum v bmap
    case runProgram bot of 
        Nothing -> return $ Right $ (insert bnum bot bmap, omap)
        Just (Left v) -> return $ Left v
        Just (Right r) -> processResult m r bnum w
step m (GiveProgram p bnum) w@(bmap,omap) = do
    bot <- giveProgram bnum p bmap
    case runProgram  bot of
        Nothing -> return $ Right $ (insert bnum bot bmap, omap)
        Just (Left v) -> return $ Left v
        Just (Right r) -> processResult m r bnum w

runProgram :: Bot -> Maybe (Either Value Result)
runProgram bot = do
    a <- valueA bot
    b <- valueB bot
    p <- program bot
    Just $ Right (runProgram' (a,b) p) 

runProgram' :: (Value, Value) -> Program -> Result
runProgram' (a,b) p =
    case compare a b of
        GT -> ((high p,a) , (low p, b))
        LT -> ((low p,a) , (high p, b))
        EQ -> undefined

processResult :: Match -> Result -> BotNum -> World -> Maybe (Either Value World)
processResult m (a,a') bnum w = 
    case processAction m a w of
        Just (Right w') -> case processAction m a' w' of
            Just (Right w''@(bmap,omap)) -> Just $ Right $ (resetBot bnum bmap, omap)
            x -> x
        x -> x

resetBot :: BotNum -> BotMap -> BotMap
resetBot bnum bmap = insert bnum (createBot bnum) bmap

checkOutput :: Match -> OutputMap -> Maybe Value
checkOutput (oa,ob,oc) omap = do
   a <- lookup' oa omap 
   b <- lookup' ob omap
   c <- lookup' oc omap
   return (a*b*c)

processAction :: Match -> Action -> World -> Maybe (Either Value World)
processAction m ((ToOutput onum), v) w@(bmap,omap) =
    let omap' = insert onum v omap in
    case checkOutput m omap' of
        Nothing -> Just $ Right $ (bmap,omap')
        Just v  -> Just $ Left v
processAction m ((ToBot bn), v) w = 
    step m (GiveValue v bn) w 

createBot :: BotNum -> Bot
createBot bn = Bot {
      botNum = bn
    , valueA = Nothing
    , valueB = Nothing
    , program = Nothing}

empty :: World
empty = ([],[])

b2p = Program { low = (ToBot 1), high = (ToBot 0) }
b2 = Bot { botNum = 2,  valueA = Just 5, valueB = Nothing, program = Just b2p }

main :: IO ()
main = do
    s <- readFile "input.txt"
    let cs = runParser commands s
    print $ run (0,1,2) cs empty
