module Iter where

lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    (\x xs ->
       case x of
         (Left x) -> x : xs
         _ -> xs)
    []

rights' :: [Either a b] -> [b]
rights' =
  foldr
    (\x xs ->
       case x of
         (Right x) -> x : xs
         _ -> xs)
    []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' =
  foldr
    (\x (lefts, rights) ->
       case x of
         (Left x) -> (x : lefts, rights)
         (Right x) -> (lefts, x : rights))
    ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (const Nothing) (Just . f) x
