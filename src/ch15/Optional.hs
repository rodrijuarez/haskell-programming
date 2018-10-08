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
