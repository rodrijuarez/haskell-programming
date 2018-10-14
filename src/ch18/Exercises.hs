module Exercises where

import Control.Monad (join, liftM2)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ f <$> x

twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a m f = f <*> m

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = liftM2 (:) (f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs (\x -> x)
