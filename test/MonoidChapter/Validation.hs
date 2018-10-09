module MonoidChapter.Validation where

import Test.QuickCheck

data Validation a b
  = Failuure a
  | Succeess b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Succeess a) <> _ = Succeess a
  _ <> (Succeess a) = Succeess a
  (Failuure a) <> _ = Failuure a

type ValidationAssoc
   = Validation String String -> Validation String String -> Validation String String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =
    frequency [(1, Failuure <$> arbitrary), (1, Succeess <$> arbitrary)]
