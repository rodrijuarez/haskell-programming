module WarmingUp where

import Control.Applicative (liftA2)
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = reverse . cap

fmapped :: [Char] -> [Char]
fmapped = fmap reverse cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> reverse

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> reverse <*> cap

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
  a <- rev
  b <- cap
  return (a, b)

tupled''' :: [Char] -> ([Char], [Char])
tupled''' = rev >>= (\r -> cap >>= (\c -> return (r, c)))

newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f <$> ra)

instance Applicative (Reader r) where
  pure f = Reader (const f)
  (<*>) (Reader f) (Reader g) = Reader (f <*> g)

instance Monad (Reader r) where
  return f = Reader (const f)
  (>>=) (Reader fa) f = Reader (\r -> (runReader (f (fa r)) r))

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+ 1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+ 1) r, length r)

fOne :: Num a => [a] -> ([a], Int)
fOne = liftA2 (,) (map (+ 1)) (length)

frooty :: Num a => [a] -> ([a], Int)
frooty = foo >>= bar
