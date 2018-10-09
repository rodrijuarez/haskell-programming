{-# LANGUAGE FlexibleInstances #-}

module MonoidChapter.Mem where

import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance (Semigroup a) => Semigroup (Mem s a) where
  a <> b =
    Mem
    { runMem =
        (\x ->
           ( (fst (runMem a $ x)) <> (fst (runMem b $ x))
           , (snd (runMem a $ (snd $ runMem b $ x)))))
    }

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem {runMem = \x -> (mempty, x)}

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Mem a b) where
  arbitrary = do
    y <- arbitrary
    return (Mem (\x -> (y, x)))

semigroupAssocMem ::
     Mem String String
  -> Mem String String
  -> Mem String String
  -> String
  -> Bool
semigroupAssocMem a b c d =
  (runMem (a <> (b <> c)) $ d) == (runMem ((a <> b) <> c) $ d)

monoidLeftIdentityForMem :: Mem String String -> String -> Bool
monoidLeftIdentityForMem m x = (runMem (mappend m mempty)) x == runMem m x

monoidRightIdentityForMem :: Mem String String -> String -> Bool
monoidRightIdentityForMem m x = (runMem (mappend mempty m)) x == runMem m x

instance Show (Mem String String) where
  show _ = "Mem String"

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  quickCheck (semigroupAssocMem)
  quickCheck (monoidLeftIdentityForMem)
  quickCheck (monoidRightIdentityForMem)
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
