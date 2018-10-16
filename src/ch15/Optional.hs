module Optional where

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (Only a) <> (Only b) = Only (a <> b)
  (Only a) <> _ = Only a
  _ <> (Only a) = Only a
  _ <> _ = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend a b = a <> b

instance Functor (Optional) where
  fmap _ (Nada) = Nada
  fmap f (Only a) = Only $ f a

instance Foldable (Optional) where
  foldr f b (Only a) = f a b
  foldr _ b (Nada) = b

instance Traversable (Optional) where
  traverse f (Only a) = Only <$> f a
  traverse _ Nada = pure Nada
