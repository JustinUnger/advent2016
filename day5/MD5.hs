module MD5 (md5,md5toString) where

import Data.Char
import Data.Word
import Data.Bits
import qualified Data.ByteString.Char8 as B

a0 = 0x67452301 :: Word32
b0 = 0xefcdab89 :: Word32
c0 = 0x98badcfe :: Word32
d0 = 0x10325476 :: Word32

f :: Word32 -> Word32 -> Word32 -> Word32
f b c d = (b .&. c) .|. ((complement b) .&. d)

g b c d = (b .&. d) .|. (c .&. (complement d))

h b c d = b `xor` c `xor` d

i b c d = c `xor` (b .|. (complement d))

-- md5 is 64 ops on 32bit words
-- for 512bit blocks

type Block = [Word32]

data MD5State = MD5State Word32 Word32 Word32 Word32 deriving Show

initial = MD5State a0 b0 c0 d0

type F = Word32 -> Word32 -> Word32 -> Word32

{-
-- one of 64 ops per md5 run
op :: MD5State -> F -> Word32 -> Word32 -> Int -> MD5State
op (MD5State a b c d) f m k shift = 
    let a' = d
        b' = ((a + (f b c d) + m + k) `rotateL` shift) + b
        c' = b
        d' = c in (MD5State a' b' c' d')
-}

op :: Int -> MD5State -> [Word32] -> MD5State
op n (MD5State a b c d) ms = 
    let a' = d
        b' = ((a + (f b c d) + m + k) `rotateL` shift) + b
        c' = b
        d' = c in (MD5State a' b' c' d')
        where f = funcs !! (n `div` 16)
              k = ks !! n
              shift = shifts !! n
              m = ms !! (msgIdx n)

msgIdx n
    | n `elem` [0..15] = n 
    | n `elem` [16..31] = (5*n+1) `mod` 16
    | n `elem` [32..47] = (3*n+5) `mod` 16
    | n `elem` [48..63] = (7*n) `mod` 16
    | otherwise = error "msgIdx kaboom"

funcs :: [F]
funcs = [f, g, h, i]

ks :: [Word32]
ks = map (\x -> floor ((fromIntegral (2^32)) * abs (sin (fromIntegral x)))) [1..64]

shifts :: [Int]
shifts = [ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
           5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
           4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
           6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ]
        

md5 :: String -> String
md5 s = md5toString $ go initial msg
    where msg = stringToWord32 $ pad s
          go s [] = s
          go s m  = go (chunk 0 s x) rest
            where (x,rest) = splitAt 16 m

chunk n s m = go n s s m -- chunk (n+1) (op n s m) m
    where go 64 (MD5State a b c d) (MD5State a' b' c' d') _ = (MD5State (a+a') (b+b') (c+c') (d+d'))
          go n s s' m = go (n+1) s (op n s' m) m

testMsg = "The quick brown fox jumps over the lazy dog"

stringToWord32 :: String -> [Word32]
stringToWord32 "" = []
stringToWord32 ss = x : stringToWord32 ss'
    where (s,ss') = splitAt 4 ss
          x       = foldr (\c w -> shiftL w 8 + ((fromIntegral . ord) c)) 0 s

pad s = s ++ padding ++ l
    where padding = '\128' : replicate (fromIntegral zeros) '\000'
          zeros   = shiftR (( 440 - len ) .&. 511) 3
          l       = length_to_chars 8 $ fromIntegral $ (length s) * 8
          len     = 8 * (length s) `mod` 512

length_to_chars :: Int -> Word64 -> String
length_to_chars 0 _ = []
length_to_chars n sz = x : length_to_chars (n-1) (shiftR sz 8)
    where x = chr $ fromIntegral (sz .&. 255)

toHex :: Word32 -> String
toHex w = swap cs
    where cs = map (\x -> hexChars !! ((shiftR (fromIntegral w) (x*4)) .&. 0xf)) [0..7]
          hexChars = [ '0'..'9' ]  ++ [ 'a' .. 'f' ]
          swap (x:y:ys) = y:x:swap ys
          swap _        = []

md5toString :: MD5State -> String
md5toString (MD5State a b c d) = toHex a ++ toHex b ++ toHex c ++ toHex d

main = do
    input <- getContents
    print $ md5 input
