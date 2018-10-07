module WordNumber where

import Data.List (intersperse)

sumTill :: (Num a, Eq a) => a -> a
sumTill 0 = 0
sumTill n = n + sumTill (n - 1)

multiplyBy :: (Integral a) => a -> a -> a
multiplyBy x 0 = 0
multiplyBy x 1 = x
multiplyBy x y = x' + multiplyBy x (y - 1)
  where
    x' = x

mc91 :: Int -> Int
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 (mc91 (x + 11))

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord 10 = "ten"

digits :: Int -> [Int]
digits x
  | div x 10 == 0 = [x]
  | otherwise = digits (div x 10) ++ [digit]
  where
    digit = mod x 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
