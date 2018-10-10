module FunctorChapter.Wrap where

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (f <$> fa)

getInt :: IO Int
getInt = fmap read getLine
