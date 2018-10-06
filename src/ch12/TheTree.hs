module TheTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f b =
  case f b of
    (Just (left, next, right)) -> Node (unfold f left) next (unfold f right)
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold
    (\x ->
       if x >= n
         then Nothing
         else Just (x + 1, x, x + 1))
    0
