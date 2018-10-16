module ApplicativeChapter.List where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Semigroup (List a) where
  (Cons f list) <> (Cons a list') = Cons f (list <> (Cons a list'))
  (Cons f list) <> Nil = Cons f list
  Nil <> (Cons f list) = Cons f list
  _ <> _ = Nil

instance Functor (List) where
  fmap _ (Nil) = Nil
  fmap f (Cons a list) = Cons (f a) (f <$> list)

-- [(+1), (+2)] <*> [1,2,3]
instance Applicative (List) where
  pure a = (Cons a Nil)
  (Cons f list) <*> allElems@(Cons _ _) =
    (f <$> allElems) <> (list <*> allElems)
  _ <*> _ = Nil

instance Monad (List) where
  return = pure
  (>>=) (Cons a l) f = (f a) <> (l >>= f)
  (>>=) (Nil) _ = Nil

instance Foldable (List) where
  foldr _ b (Nil) = b
  foldr f b (Cons a rest) = f a (foldr f b rest)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency
      [ ( 1
        , do x <- arbitrary
             y <- arbitrary
             return (Cons x y))
      , (1, return Nil)
      ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ functor (undefined :: List (Int, String, Int))

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a l) = Cons a (take' (n - 1) l)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take' 3000 l
      ys' =
        let (ZipList' l) = ys
        in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Semigroup (ZipList' a) where
  (ZipList' l) <> (ZipList' l') = ZipList' $ l <> l'

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  (<*>) (ZipList' (Cons f l)) (ZipList' (Cons x l')) =
    ZipList' (Cons (f x) Nil) <> (ZipList' (l) <*> ZipList' (l'))
  (<*>) _ _ = ZipList' (Nil)
