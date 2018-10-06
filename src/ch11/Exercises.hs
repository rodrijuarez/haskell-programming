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
capitalizeWords xs = [(x, capitalizeWord x) | x <- words xs]

capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph ('.':x:xs) = '.' : toUpper x : xs
capitalizeParagraph (x:xs) = x : capitalizeParagraph xs
