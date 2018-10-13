module MonoidChapter.Four where

import Laws.Functor
import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type FourAssoc
   = Four [Int] String String [Int] -> Four [Int] String String [Int] -> Four [Int] String String [Int] -> Bool

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

data Four' a b =
  Four' a
        a
        a
        b
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
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (Four a b c fd) <*> (Four a' b' c' d) =
    Four (a <> a') (b <> b') (c <> c') (fd d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure d = Four' mempty mempty mempty d
  (Four' a b c fd) <*> (Four' a' b' c' d) =
    Four' (a <> a') (b <> b') (c <> c') (fd d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
  quickCheck (monoidRightIdentity :: Four String String String String -> Bool)
  quickCheck (functorIdentity :: Four String String String String -> Bool)
  quickCheck
    (functorCompose' :: Four Int String String String -> Fun String String -> Fun String String -> Bool)
  quickCheck (functorIdentity :: Four' String String -> Bool)
  quickCheck
    (functorCompose' :: Four' String String -> Fun String String -> Fun String String -> Bool)
  quickBatch $
    applicative
      (undefined :: Four (String, String, String) (String, String, String) ( String
                                                                           , String
                                                                           , String) ( String
                                                                                     , String
                                                                                     , String))
  quickBatch $
    applicative
      (undefined :: Four' (String, String, String) (String, String, String))
