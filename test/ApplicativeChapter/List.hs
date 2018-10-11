module ApplicativeChapter.List where

import Control.Applicative

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show)

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
  (Cons f list) <*> all@(Cons a list') = (f <$> all) <> (list <*> all)
  (Cons f list) <*> Nil = Nil
  Nil <*> (Cons f list) = Nil
  _ <*> _ = Nil
