data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

type OrAssoc = Or [Int] String -> Or [Int] String -> Or [Int] String -> Bool

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  (Fst a) <> _ = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, fmap Fst arbitrary), (1, fmap Snd arbitrary)]
