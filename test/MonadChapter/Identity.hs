module MonadChapter.Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monad)

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity fa) (Identity a) = Identity $ fa a

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Identity (String, Int, String))
  quickBatch $ applicative (undefined :: Identity (String, Int, String))
  quickBatch $ monad (undefined :: Identity (String, Int, String))
