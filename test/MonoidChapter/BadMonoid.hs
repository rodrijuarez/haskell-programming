module MonoidChapter.BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  (<>) Fools Fools = Fools
  (<>) _ _ = Twoo

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = quickBatch (monoid Fools)
