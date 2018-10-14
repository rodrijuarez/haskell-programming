module MonadChapter.Huum where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (monad)

data PtEither a b
  = Lefft b
  | Rait a
  deriving (Eq, Show)

instance Functor (PtEither a) where
  fmap f (Lefft b) = Lefft (f b)
  fmap _ (Rait a) = Rait a

instance (Monoid a) => Applicative (PtEither a) where
  pure b = Lefft b
  (<*>) (Lefft fb) (Lefft b) = Lefft (fb b)
  (<*>) (Rait a) (Rait a') = Rait (a <> a')
  (<*>) (Rait a) _ = Rait a
  (<*>) _ (Rait a') = Rait a'

instance (Monoid a) => Monad (PtEither a) where
  return = pure
  (>>=) (Lefft b) f = f b
  (>>=) (Rait a) _ = Rait a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PtEither a b) where
  arbitrary = frequency [(1, Lefft <$> arbitrary), (1, Rait <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PtEither a b) where
  (=-=) = eq

main :: IO ()
main = quickBatch $ monad (undefined :: PtEither String (String, Int, String))
