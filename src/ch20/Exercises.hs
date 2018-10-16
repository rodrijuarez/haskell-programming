module Foldable where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap (Sum)

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap (Product)

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (== x))

length :: (Foldable t) => t a -> Int
length xs = foldr (const (+ 1)) 0 xs

data Min' a = Min'
  { getMin :: Maybe a
  } deriving (Eq, Show)

instance (Ord a) => Semigroup (Min' a) where
  (<>) (Min' Nothing) m = m
  (<>) m (Min' Nothing) = m
  (<>) (Min' a) (Min' a') = Min' $ min a a'

instance Ord a => Monoid (Min' a) where
  mempty = Min' Nothing

-- This code was stolen
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' t = getMin $ foldMap (Min' . Just) t

data Max' a = Max'
  { getMax :: Maybe a
  } deriving (Eq, Show)

instance (Ord a) => Semigroup (Max' a) where
  (<>) (Max' Nothing) m = m
  (<>) m (Max' Nothing) = m
  (<>) (Max' a) (Max' a') = Max' $ max a a'

instance Ord a => Monoid (Max' a) where
  mempty = Max' Nothing

-- This one too
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' t = getMax $ foldMap (Max' . Just) t

toList :: (Foldable t) => t a -> [a]
toList x = foldr (:) [] x

fold :: (Foldable t, Monoid m) => t m -> m
fold x = foldMap id x

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f x = foldr ((<>) . f) mempty x

-- My folds
data Constant a b =
  Constant a
  deriving (Show)

instance Foldable (Constant a) where
  foldr _ b _ = b

data Two a b =
  Two a
      b
  deriving (Show)

instance Foldable (Two a) where
  foldr f x (Two _ a) = f a x

data Three a b c =
  Three a
        b
        c
  deriving (Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f x (Three _ _ a) = f a x

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Three' a b =
  Three' a
         b
         b
  deriving (Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> (f b) <*> f b'

data Four' a b =
  Four' a
        b
        b
        b
  deriving (Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

filterF ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs =
  foldMap
    (\x ->
       if (f x)
         then pure x
         else mempty)
    xs
