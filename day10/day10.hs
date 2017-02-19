import NanoParsec (runParser)
import Parser (Command(..),commands, BotNum, Value, Program(..), Action, Destination(..))

insert :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
insert k v [] = [(k,v)]
insert k v (kv:kvs) = if k == fst kv then (k,v) : kvs else kv : insert k v kvs

lookup' :: Eq k => k -> [(k,v)] -> Maybe v
lookup' _ [] = Nothing
lookup' k (x:xs) = let (k',v) = x in if k' == k then Just v else lookup k xs

data State s a = State { runState :: s -> (a, s) }

type OutputNum = Integer
type BotMap = [(BotNum,Bot)]

data Bot = Bot { 
      botNum :: BotNum
    , valueA :: Maybe Value
    , valueB :: Maybe Value
   , program :: Maybe Program } deriving Show

{-
data Program = Program { high :: Destination, low :: Destination } deriving Show

data Destination = ToBot BotNum | ToOutput OutputNum deriving Show

type Action = (Destination,Value)
-}
type Result = (Action,Action)
type Match = BotNum

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


run :: (Value,Value) -> [Command] -> BotMap -> Either BotNum BotMap
run m [] bmap  = Right bmap
run m (c:cs) bmap = case step m c bmap of
    Nothing -> error $ "command error: " ++ show c
    Just (Left bn) -> Left bn
    Just (Right bmap') -> run m cs bmap'

step :: (Value, Value) -> Command -> BotMap -> Maybe (Either BotNum BotMap)
step m (GiveValue v bnum) bmap = do
    bot <- giveValue bnum v bmap
    case runProgram m bot of 
        Nothing -> return $ Right $ insert bnum bot bmap
        Just (Left bn) -> return $ Left bn
        Just (Right r) -> processResult m r bnum bmap
step m (GiveProgram p bnum) bmap = do
    bot <- giveProgram bnum p bmap
    case runProgram m bot of
        Nothing -> return $ Right $ insert bnum bot bmap
        Just (Left bn) -> return $ Left bn
        Just (Right r) -> processResult m r bnum bmap

runProgram :: (Value, Value) -> Bot -> Maybe (Either BotNum Result)
runProgram (a',b') bot = do
    a <- valueA bot
    b <- valueB bot
    if a == a' && b == b' then
        Just $ Left $ botNum bot
    else do
        p <- program bot
        Just $ Right (runProgram' (a,b) p) 

runProgram' :: (Value, Value) -> Program -> Result
runProgram' (a,b) p =
    case compare a b of
        GT -> ((high p,a) , (low p, b))
        LT -> ((low p,a) , (high p, b))
        EQ -> undefined

processResult :: (Value, Value) -> Result -> BotNum -> BotMap -> Maybe (Either BotNum BotMap)
processResult m (a,a') bnum bmap = 
    case processAction m a bnum bmap of
        Nothing -> Nothing
        Just (Left bn) -> Just (Left bn)
        Just (Right bmap') -> case processAction m a' bnum bmap' of
            Nothing -> Nothing
            Just (Left bn) -> Just (Left bn)
            Just (Right bmap'') -> Just $ Right $ resetBot bnum bmap''

resetBot :: BotNum -> BotMap -> BotMap
resetBot bnum bmap = insert bnum (createBot bnum) bmap

processAction :: (Value, Value) -> Action -> BotNum -> BotMap -> Maybe (Either BotNum BotMap)
processAction m ((ToOutput _), _) bnum bmap = Just $ Right $ bmap
processAction m ((ToBot bn), v) bnum bmap = 
    step m (GiveValue v bn) bmap 

createBot :: BotNum -> Bot
createBot bn = Bot {
      botNum = bn
    , valueA = Nothing
    , valueB = Nothing
    , program = Nothing}

empty = []

b2p = Program { low = (ToBot 1), high = (ToBot 0) }
b2 = Bot { botNum = 2,  valueA = Just 5, valueB = Nothing, program = Just b2p }

main :: IO ()
main = do
    s <- readFile "input.txt"
    let cs = runParser commands s
    print $ run (61,17) cs empty
