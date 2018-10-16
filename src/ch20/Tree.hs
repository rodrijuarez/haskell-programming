module Tree where

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node nl a nr) = Node (f <$> nl) (f a) (f <$> nr)

instance Foldable Tree where
  foldMap f (Empty) = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node nl a nr) = (foldMap f nl) <> (f a) <> (foldMap f nr)

instance Traversable Tree where
  traverse f (Empty) = pure Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node nl a nr) =
    Node <$> (traverse f nl) <*> (f a) <*> (traverse f nr)
