module MonoidChapter.BoolConj where

import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary :: Gen Bool
    return (BoolConj x)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
