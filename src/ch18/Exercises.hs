module Exercises where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ f <$> x

twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []
