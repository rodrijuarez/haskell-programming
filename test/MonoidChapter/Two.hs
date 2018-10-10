module MonoidChapter.Two where

import Laws.Functor
import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck

type TwoAssoc
   = Two String String -> Two String String -> Two String String -> Bool

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = (Two mempty mempty)
  mappend = (<>)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (functorIdentity :: Two String String -> Bool)
  quickCheck
    (functorCompose' :: Two Int String -> Fun String String -> Fun String String -> Bool)
