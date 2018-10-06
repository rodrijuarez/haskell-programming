module Maitbe where

import Control.Monad (foldM)
     -- >>> isJust (Just 1)
     -- True
     -- >>> isJust Nothing
     -- False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False
     -- >>> isNothing (Just 1)
     -- False
     -- >>> isNothing Nothing
     -- True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- >>> mayybee 0 (+1) Nothing
--0
-- >>> mayybee 0 (+1) (Just 1)
--2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ _ = b

-- >>> fromMaybe 0 Nothing --0
-- -- >>> fromMaybe 0 (Just 1) --1
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a _ = a

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe ((Just x):xs) = (x :) <$> (flipMaybe xs)
