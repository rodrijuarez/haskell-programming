module Exercises where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (DbDate time:xs) = time : filterDbDate xs
filterDbDate (_:xs) = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber x:xs) = x : filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral total) / (fromIntegral amountOfNumbers)
  where
    numbers = filterDbNumber xs
    total = sum numbers
    amountOfNumbers = length numbers

fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 $ 1 : scanl (+) 1 fibs

fibsLess = filter (< 100) $ 1 : scanl (+) 1 fibs

factorial = 1 : scanl (*) 2 factorial

tops = "pbtdkg"

vowels = "aeiou"

zipper = [(x, y, z) | x <- tops, y <- vowels, z <- tops]

zipperP = [(x, y, z) | x <- tops, y <- vowels, z <- tops, x == 'p']
