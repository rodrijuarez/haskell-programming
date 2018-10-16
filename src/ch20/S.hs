module S where

data S n a =
  S (n a)
    a

-- to make it easier, we'll give you the constraints.
instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldr f b (S na a) = f a (foldr f b na)

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> (traverse f na) <*> (f a)
