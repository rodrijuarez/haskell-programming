module Exercises where

import Data.List

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Ord a, Enum a) => a -> a -> [a]
eft a b
  | a == b = [b]
  | a > b = []
  | otherwise = a : eft (succ a) b

-- Thy Fearful Symettry
--
myWords :: [Char] -> [[Char]]
myWords = breakBy ' '

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
     -- putStrLn sentences -- should print
     -- Tyger Tyger, burning bright
     -- In the forests of the night
     -- What immortal hand or eye
     -- Could frame thy fearful symmetry?
     -- Implement this

myLines :: String -> [String]
myLines = breakBy '\n'
     -- What we want 'myLines sentences' to equal
     --

breakBy :: Char -> [Char] -> [[Char]]
breakBy _ "" = []
breakBy char (x:xs)
  | char == x = breakBy char xs
  | otherwise = [first] ++ breakBy char rest
  where
    first = takeWhile (/= char) xs
    rest = dropWhile (/= char) xs

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True
myOr (False:xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x = True
  | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:ys) = x ++ squish ys

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f xs = concat $ map f xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = last $ sortBy f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = head $ sortBy f xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
