{-# LANGUAGE FlexibleInstances #-}

module Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a
  = NichtSoGut e
  | AllesGut a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap f (AllesGut a) = AllesGut $ f a
  fmap _ (NichtSoGut e) = NichtSoGut e

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure a = AllesGut a
  (AllesGut f) <*> a = f <$> a
  (NichtSoGut f) <*> (NichtSoGut a) = NichtSoGut (f <> a)
  (NichtSoGut f) <*> _ = NichtSoGut f

-- Examples
data Errors
  = DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

success :: Validation [Errors] Int
success = AllesGut (+ 1) <*> AllesGut 1

r1 = success == AllesGut 2

failure = AllesGut (+ 1) <*> NichtSoGut [StackOverflow]

r2 = failure == NichtSoGut [StackOverflow]

failure' :: Validation [Errors] Int
failure' = NichtSoGut [StackOverflow] <*> AllesGut (+ 1)

r3 = failure' == NichtSoGut [StackOverflow]

failures :: Validation [Errors] Int
failures = NichtSoGut [MooglesChewedWires] <*> NichtSoGut [StackOverflow]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =
    frequency [(1, AllesGut <$> arbitrary), (1, NichtSoGut <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq
