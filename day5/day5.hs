import Crypto.Hash.MD5
import Data.ByteString.Base16
import Data.ByteString.Char8 (ByteString,pack,unpack)
import Data.List

keys :: String -> [ByteString]
keys k = map (pack . (k ++)) (map show [0..])

hashKeys = map (unpack . encode . hash)

password = take 8 . map (!! 5) . filter (isPrefixOf "00000") . hashKeys . keys

main = do
    print $ password "ojvtpuvg"


