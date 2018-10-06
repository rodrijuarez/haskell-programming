module Ciphers where

import Data.Char
import System.IO

--vigenere :: String -> String -> String
vigenere phrase keyword = unwords $ map (map (fst . encodeFirst)) wordsWithKeys
  where
    wordsWithKeys = map (alignKeys keyword) (words phrase)

alignKeys :: String -> String -> [(Char, Char)]
alignKeys keyword word = zip word $ cycle keyword

encodeFirst :: (Char, Char) -> (Char, Char)
encodeFirst (x, y) = (chr $ ord x + distance, y)
  where
    distance = distanceFromTo 'a' y

distanceFromTo :: Char -> Char -> Int
distanceFromTo x y = (ord y) - (ord x)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Give me your keyword:"
  keyword <- getLine
  putStrLn "Give me your phrase:"
  phrase <- getLine
  putStrLn $ "Result: " ++ vigenere phrase keyword
  return ()
