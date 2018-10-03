module Ciphers where

import Data.Char

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
