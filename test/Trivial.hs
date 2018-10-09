{-# LANGUAGE FlexibleInstances #-}

module Trivial where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.Property.Monoid

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap (Identity) arbitrary

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

-- Two!!!
type TwoAssoc = Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

-- Three!!!
type ThreeAssoc
   = Three [Int] String String -> Three [Int] String String -> Three [Int] String String -> Bool

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

-- Four!!!
type FourAssoc
   = Four [Int] String String [Int] -> Four [Int] String String [Int] -> Four [Int] String String [Int] -> Bool

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four w x y z) <> (Four w' x' y' z') =
    Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

-- BoolConj
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary :: Gen Bool
    return (BoolConj x)

-- BoolDisj
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary :: Gen Bool
    return (BoolDisj x)

-- Or
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

-- Combine
newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

type CombineAssoc
   = Combine String String -> Combine String String -> Combine String String -> String -> Bool

instance (Semigroup b) => Semigroup (Combine a b) where
  a <> b = Combine {unCombine = (\x -> (unCombine a $ x) <> (unCombine b $ x))}

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- promote (\a -> coarbitrary a arbitrary)
    return (Combine f)

semigroupAssocForCombine ::
     Combine String String
  -> Combine String String
  -> Combine String String
  -> String
  -> Bool
semigroupAssocForCombine a b c d =
  (unCombine (a <> (b <> c)) $ d) == (unCombine ((a <> b) <> c) $ d)

instance Show (Combine String String) where
  show _ = "Combine String String"

-- Comp
newtype Comp a = Comp
  { comp :: (a -> a)
  }

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

instance (Semigroup a) => Semigroup (Comp a) where
  a <> b = Comp {comp = (\x -> (comp a $ x) <> (comp b $ x))}

instance (CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    return (Comp (\x -> x))

semigroupAssocComp :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssocComp a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Comp
data Validation a b
  = Failuure a
  | Succeess b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Succeess a) <> _ = Succeess a
  _ <> (Succeess a) = Succeess a
  (Failuure a) <> _ = Failuure a

type ValidationAssoc
   = Validation String String -> Validation String String -> Validation String String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =
    frequency [(1, fmap Failuure arbitrary), (1, fmap Succeess arbitrary)]

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  verboseCheck (semigroupAssocForCombine)
  quickCheck (semigroupAssoc :: ValidationAssoc)
