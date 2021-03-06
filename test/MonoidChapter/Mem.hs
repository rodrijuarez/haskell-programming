{-# LANGUAGE FlexibleInstances #-}

module MonoidChapter.Mem where

import Control.Applicative
import Laws.Monoid
import Laws.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem mem1) <> (Mem mem2) =
    Mem $ \s ->
      let (a1, _) = mem1 s
          (a2, s1) = mem2 s
          (_, s2) = mem1 s1
      in (a2 <> a1, s2)

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

f2' :: Mem Int String
f2' = Mem $ \s -> ("bye", s + 1)

main :: IO ()
main = do
  quickCheck (semigroupAssocMem)
  quickCheck (monoidLeftIdentityForMem)
  quickCheck (monoidRightIdentityForMem)
  print $ runMem (f' <> f2') 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
