import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Function
import Data.Word
import Data.Char
import Data.Bits
import Data.List
import Control.Monad


-- After attempts at doing everything in ByteString, I decided the added
-- complexity is not worth it -- for such small datasets.
--
-- As I'm still learning Haskell, after completing set1 I went on to look
-- for other solutions to compare against. I was humbled by how crude my
-- code looks comparing to others and pertly borrowed the Tom Szczarkowski
-- file structure:
-- https://github.com/tom-szczarkowski/matasano-crypto-puzzles-solutions

main :: IO [()]
main = ch1 >> ch2 >> ch3 >> ch4

-- Challenge 1
ch1 :: IO [()]
ch1 = do
    putStrLn "\nCH1-1: Convert hex to base64"
    let x = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    let y = toB16 . fromHex $ x
    mapM putStrLn [x,y]

fromHex :: String -> B.ByteString
fromHex = fst . B16.decode . C.pack

toHex :: B.ByteString -> String
toHex = C.unpack . B16.encode

toB16 :: B.ByteString -> String
toB16 = C.unpack . B64.encode

-- Challenge 2
ch2 :: IO [()]
ch2 = do
    putStrLn "\nCH1-2: Fixed XOR"
    let x1 = fromHex "1c0111001f010100061a024b53535009181c"
    let x2 = fromHex "686974207468652062756c6c277320657965"
    let y = B.pack $ xorB x1 x2
    mapM (putStrLn . toHex) [x1, x2, y]

xorB :: B.ByteString -> B.ByteString -> [Word8]
xorB =  B.zipWith xor


-- Challenge 3
ch3 :: IO [()]
ch3 = do
    putStrLn "\nCH1-3: Single-byte XOR cipher"
    let x = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    let y = decryptGuess $ x
    mapM putStrLn [x, y]

decryptGuess :: String -> String
decryptGuess = C.unpack . minimumBy (compare `on` scoreText) . bruteXor . fromHex

-- Guess the key by xoring to ' '
bruteXor :: B.ByteString -> [B.ByteString]
bruteXor s = [B.map (xor $ xor 32 e) s | e <- guesses s]
    where
      guesses = fmap B.head . sortBy (compare `on` negate . B.length) . B.group . B.sort

-- Compare sample top N characters to English standard
scoreText :: C.ByteString -> Double
scoreText = sum . C.foldr ((:) . log . rate . toLower) []

rate :: Char -> Double
rate c
    | isSpace c = freqSpace
    | ord c > 96 && ord c < 123 = freqAZ !! (ord c - 97)
    | isDigit c || isControl c = freqOther
    | otherwise = 0.0001
    where
      freqAZ = [0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974, 0.00074]
      freqSpace = 0.13
      freqOther = 0.085


-- Chi-squared test
chiSqr :: [Double] -> [Double] -> Double
chiSqr xs ys = foldr (+) 0 $ zipWith (\x y -> ((x - y) ^ 2) / y) xs ys


-- Challenge 4
ch4 :: IO [()]
ch4 = do
    putStrLn "\nCH1-4: Detect single-character XOR"
    x <- liftM (map decryptGuess . lines) $ readFile "ch4-input.txt"
    mapM print x



