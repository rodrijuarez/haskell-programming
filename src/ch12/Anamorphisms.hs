module Anamorphisms where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    (Just (x, y)) -> x : myUnfoldr f y
    Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x
