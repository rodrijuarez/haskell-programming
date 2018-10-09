module MonoidChapter.Four where

import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck

type FourAssoc
   = Four [Int] String String [Int] -> Four [Int] String String [Int] -> Four [Int] String String [Int] -> Bool

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four w x y z) <> (Four w' x' y' z') =
    Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = (Four mempty mempty mempty mempty)
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
  quickCheck (monoidRightIdentity :: Four String String String String -> Bool)
