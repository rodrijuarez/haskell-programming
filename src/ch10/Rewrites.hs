module Rewrites where

myAnd :: [Bool] -> Bool
myAnd = foldl (&&) True

myOr :: [Bool] -> Bool
myOr = foldl (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

myElem :: Eq a => a -> [a] -> Bool
myElem elem xs = foldr (\x y -> x == elem || y) False xs

myReverse :: [a] -> [a]
myReverse xs = foldr (\x y -> y ++ [x]) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x y -> f x : y) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs =
  foldr
    (\x y ->
       if f x
         then x : y
         else y)
    []
    xs

squish :: [[a]] -> [a]
squish xs = foldr (++) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr ((++) . f) [] xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (init:rest) =
  foldl
    (\x y ->
       if (f x y) == GT
         then x
         else y)
    init
    rest

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (init:rest) =
  foldl
    (\x y ->
       if (f x y) == LT
         then x
         else y)
    init
    rest
