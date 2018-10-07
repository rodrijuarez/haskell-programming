module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Pair a b =
  Pair a
       b
  deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

pairGentIntString :: Gen (Pair Int String)
pairGentIntString = pairGen

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

main :: IO ()
main = do
  sample pairGentIntString
