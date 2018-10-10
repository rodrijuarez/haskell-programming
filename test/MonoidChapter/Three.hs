module MonoidChapter.Three where

import Laws.Functor
import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck

type ThreeAssoc
   = Three [Int] String String -> Three [Int] String String -> Three [Int] String String -> Bool

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = (Three mempty mempty mempty)
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
  quickCheck (monoidRightIdentity :: Three String String String -> Bool)
  quickCheck (functorIdentity :: Three String String String -> Bool)
  quickCheck
    (functorCompose' :: Three Int String String -> Fun String String -> Fun String String -> Bool)
  quickCheck (functorIdentity :: Three' Int String -> Bool)
  quickCheck
    (functorCompose' :: Three' Int String -> Fun String String -> Fun String String -> Bool)
