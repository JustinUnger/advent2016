import Crypto.Hash.MD5
import Data.ByteString.Base16
import Data.ByteString.Char8 (ByteString,pack,unpack)
import Data.Function
import Data.List

keys :: String -> [ByteString]
keys k = map (pack . (k ++)) (map show [0..])

hashKeys = map (unpack . encode . hash)

myFilter = (\x -> x !! 5 >= '0' && x !! 5 <= '7')

-- this is fugly, don't show this to anyone.
-- how to sort this and dedup lazily?
 
password :: String -> String
password = map snd . take 8 . nubBy ((==) `on` fst) . sortOn fst . take 16 . map (\x -> (read [(x !! 5)] :: Int,x !! 6)) . filter myFilter . filter (isPrefixOf "00000") . hashKeys . keys

main = do
    print $ password "ojvtpuvg"


