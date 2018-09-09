module Reverse where

rvrs :: String -> String
rvrs xs = first ++ " " ++ second ++ " " ++ third
  where
    first = drop 9 xs
    second = [(xs !! 6), (xs !! 7)]
    third = take 5 xs
