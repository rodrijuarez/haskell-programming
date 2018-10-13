module Pair where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a =
  Pair a
       a
  deriving (Show, Eq)

instance Functor (Pair) where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative (Pair) where
  pure a = Pair a a
  (Pair fa fb) <*> (Pair a b) = Pair (fa a) (fb b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq
