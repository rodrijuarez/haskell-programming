module Exercises where

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf sub@(x:xs) (y:ys) =
  if x /= y
    then isSubsequenceOf sub ys
    else isSubsequenceOf xs ys
