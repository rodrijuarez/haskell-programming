{-# LANGUAGE FlexibleInstances #-}

module MonoidChapter.BoolConj where

import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

type CombineAssoc
   = Combine String String -> Combine String String -> Combine String String -> String -> Bool

instance (Semigroup b) => Semigroup (Combine a b) where
  a <> b = Combine {unCombine = (\x -> (unCombine a $ x) <> (unCombine b $ x))}

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (mempty)
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- promote (\a -> coarbitrary a arbitrary)
    return (Combine f)

semigroupAssocForCombine ::
     Combine String String
  -> Combine String String
  -> Combine String String
  -> String
  -> Bool
semigroupAssocForCombine a b c d =
  (unCombine (a <> (b <> c)) $ d) == (unCombine ((a <> b) <> c) $ d)

monoidLeftIdentityForCombine :: Combine String String -> String -> Bool
monoidLeftIdentityForCombine m x =
  (unCombine (mappend m mempty)) x == unCombine m x

monoidRightIdentityForCombine :: Combine String String -> String -> Bool
monoidRightIdentityForCombine m x =
  (unCombine (mappend mempty m)) x == unCombine m x

instance Show (Combine String String) where
  show _ = "Combine String String"

main :: IO ()
main = do
  quickCheck (semigroupAssocForCombine)
  quickCheck (monoidLeftIdentityForCombine)
  quickCheck (monoidRightIdentityForCombine)
