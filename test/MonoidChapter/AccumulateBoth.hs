module MonoidChapter.AccumulateBoth where

import Laws.Semigroup
import MonoidChapter.Validation
import Test.QuickCheck

data AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Succeess a)) <> (AccumulateBoth (Succeess b)) =
    AccumulateBoth (Succeess (a <> b))
  _ <> (AccumulateBoth (Succeess a)) = AccumulateBoth (Succeess a)
  (AccumulateBoth (Succeess a)) <> _ = AccumulateBoth (Succeess a)
  (AccumulateBoth (Failuure a)) <> _ = AccumulateBoth (Failuure a)

type AccumulateBothAssoc
   = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    x <- arbitrary
    return $ AccumulateBoth x

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
