module MonoidChapter.Three where

import Laws.Functor
import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (<*>) (Three a b fc) (Three a' b' c) = Three (a <> a') (b <> b') (fc c)

instance (Monoid a) => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' a fb fc) (Three' a' b c) = Three' (a <> a') (fb b) (fc c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

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
  quickBatch $
    applicative
      (undefined :: Three (String, String, String) (String, String, String) ( String
                                                                            , String
                                                                            , String))
  quickBatch $
    applicative
      (undefined :: Three' (String, String, String) (String, String, String))
