module Exercises where

import Data.Char (toUpper)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf sub@(x:xs) (y:ys) =
  if x /= y
    then isSubsequenceOf sub ys
    else isSubsequenceOf xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = [(x, capitalize x) | x <- words xs]
  where
    capitalize (x:xs) = toUpper x : xs
