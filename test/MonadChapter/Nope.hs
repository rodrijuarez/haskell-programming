module Nope where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (monad)

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor (Nope) where
  fmap _ _ = NopeDotJpg

instance Applicative (Nope) where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad (Nope) where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

main :: IO ()
main = quickBatch $ monad (undefined :: Nope (String, Int, Int))
