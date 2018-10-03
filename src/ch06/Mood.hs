module Mood where

import Data.List (sort)

data Mood
  = Blah
  | Meh

instance Show Mood where
  show _ = "Blaaah"

type Subject = String

type Verb = String

type Object = String

data Sentence =
  Sentence Subject
           Verb
           Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = f y * fromInteger x

pal xs
  | xs == reverse xs = True
  | otherwise = False

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y z
  | z = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print (roundTrip 4 :: Int)
  print (id 4)
