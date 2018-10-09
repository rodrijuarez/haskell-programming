{-# LANGUAGE FlexibleInstances #-}

module MonoidChapter.Comp where

import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

newtype Comp a = Comp
  { comp :: (a -> a)
  }

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

instance (Semigroup a) => Semigroup (Comp a) where
  a <> b = Comp {comp = (\x -> (comp a $ x) <> (comp b $ x))}

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp (mempty)

instance (CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    return (Comp (\x -> x))

semigroupAssocComp ::
     Comp String -> Comp String -> Comp String -> String -> Bool
semigroupAssocComp a b c d =
  (comp (a <> (b <> c)) $ d) == (comp ((a <> b) <> c) $ d)

monoidLeftIdentityForComp :: Comp String -> String -> Bool
monoidLeftIdentityForComp m x = (comp (mappend m mempty)) x == comp m x

monoidRightIdentityForComp :: Comp String -> String -> Bool
monoidRightIdentityForComp m x = (comp (mappend mempty m)) x == comp m x

instance Show (Comp String) where
  show _ = "Comp String"

main :: IO ()
main = do
  quickCheck (semigroupAssocComp)
  quickCheck (monoidLeftIdentityForComp)
  quickCheck (monoidRightIdentityForComp)
