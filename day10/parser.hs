module Parser where

import NanoParsec
import Control.Applicative

type Value = Int
type BotNum = Int
type OutputNum = Int

data Program = Program { low :: Destination, high :: Destination } deriving (Eq, Show)

type Action = (Destination,Value)
type Result = (Action,Action)

data Destination = ToBot BotNum | ToOutput OutputNum deriving (Eq, Show)

data Command = GiveValue Value BotNum
             | GiveProgram Program BotNum deriving (Eq, Show)

destination :: Parser Destination
destination = do
    s <- string "bot" <|> string "output"
    case s of 
        "bot" -> do
            char ' '
            bn <- natural
            return $ ToBot bn
        "output" -> do
            char ' '
            on <- natural
            return $ ToOutput on

giveProgram :: Parser Command
giveProgram = do
    string "bot "
    bn <- natural
    string " gives low to "
    low <- destination
    string " and high to "
    high <- destination
    some (char '\n')
    return $ GiveProgram (Program low high) bn

giveValue :: Parser Command
giveValue = do
    string "value "
    v <- natural
    string " goes to bot "
    bn <- natural
    some (char '\n')
    return $ GiveValue v bn

commands :: Parser [Command]
commands = many $ giveProgram <|> giveValue

main :: IO ()
main = do
    s <- readFile "sample.txt"
    print $ runParser commands s

