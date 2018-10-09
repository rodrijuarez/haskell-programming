module MonoidChapter.AccumulateRight where

import Laws.Semigroup
import MonoidChapter.Validation
import Test.QuickCheck

data AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Succeess a)) <> (AccumulateRight (Succeess b)) =
    AccumulateRight (Succeess (a <> b))
  _ <> (AccumulateRight (Succeess a)) = AccumulateRight (Succeess a)
  (AccumulateRight (Succeess a)) <> _ = AccumulateRight (Succeess a)
  (AccumulateRight (Failuure a)) <> _ = AccumulateRight (Failuure a)

type AccumulateRightAssoc
   = AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    x <- arbitrary
    return $ AccumulateRight x

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
