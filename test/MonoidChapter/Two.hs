module MonoidChapter.Two where

import Laws.Functor
import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

instance (Monoid a) => Applicative (Two a) where
  pure a = Two mempty a
  (Two a fb) <*> (Two a' b) = Two (a <> a') (fb b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (functorIdentity :: Two String String -> Bool)
  quickCheck
    (functorCompose' :: Two Int String -> Fun String String -> Fun String String -> Bool)
  quickBatch $
    applicative
      (undefined :: Two (String, String, String) (String, String, String))
