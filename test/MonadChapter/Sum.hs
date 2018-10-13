module MonadChapter.Sum where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First x) = First x

instance Applicative (Sum a) where
  pure b = Second b
  (Second fb) <*> (Second b) = Second $ fb b
  (First fa) <*> _ = First fa
  _ <*> (First a) = First a

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary), (1, Second <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Sum String (Int, String, Int))
  quickBatch $ applicative (undefined :: Sum String (Int, String, Int))
  quickBatch $ monad (undefined :: Sum String (Int, String, Int))
