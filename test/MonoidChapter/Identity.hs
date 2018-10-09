module MonoidChapter.Identity where

import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap (Identity) arbitrary

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = (Identity mempty)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
