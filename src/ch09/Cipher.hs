module Ciphere where

import Data.Char

caesar :: [Char] -> [Char]
caesar "" = ""
caesar (x:xs)
  | isUpper x = shiftTill x 90 : caesar xs
  | otherwise = shiftTill x 122 : caesar xs

shiftTill :: Char -> Int -> Char
shiftTill char to = chr xord'
  where
    xord = ord char
    xord' =
      if xord + 3 > to
        then xord - 23
        else xord + 3
