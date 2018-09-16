module SyntaxFixes where

x = (+)

f xs = w `x` 1
  where
    w = length xs

myId x = x

myHead (x:xs) = x

f2 (a, b) = a
