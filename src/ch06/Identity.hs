module Identity where

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity x) (Identity y) = x == y
