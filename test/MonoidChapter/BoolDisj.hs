module MonoidChapter.BoolDisj where

import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary :: Gen Bool
    return (BoolDisj x)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
