{-# LANGUAGE FlexibleInstances #-}

module Trivial where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

--import Test.QuickCheck.Property.Monoid
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap (Identity) arbitrary

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = mempty `mappend` m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = m `mappend` mempty == m

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = (Identity mempty)
  mappend = (<>)

-- Two!!!
type TwoAssoc
   = Two String String -> Two String String -> Two String String -> Bool

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = (Two mempty mempty)
  mappend = (<>)

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

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = (Three mempty mempty mempty)
  mappend = (<>)

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

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = (Four mempty mempty mempty mempty)
  mappend = (<>)

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

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

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

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

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

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (mempty)
  mappend = (<>)

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

monoidLeftIdentityForCombine :: Combine String String -> String -> Bool
monoidLeftIdentityForCombine m x =
  (unCombine (mappend m mempty)) x == unCombine m x

monoidRightIdentityForCombine :: Combine String String -> String -> Bool
monoidRightIdentityForCombine m x =
  (unCombine (mappend mempty m)) x == unCombine m x

instance Show (Combine String String) where
  show _ = "Combine String String"

-- Comp
newtype Comp a = Comp
  { comp :: (a -> a)
  }

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

instance (Semigroup a) => Semigroup (Comp a) where
  a <> b = Comp {comp = (\x -> (comp a $ x) <> (comp b $ x))}

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp (mempty)

instance (CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    return (Comp (\x -> x))

semigroupAssocComp ::
     Comp String -> Comp String -> Comp String -> String -> Bool
semigroupAssocComp a b c d =
  (comp (a <> (b <> c)) $ d) == (comp ((a <> b) <> c) $ d)

monoidLeftIdentityForComp :: Comp String -> String -> Bool
monoidLeftIdentityForComp m x = (comp (mappend m mempty)) x == comp m x

monoidRightIdentityForComp :: Comp String -> String -> Bool
monoidRightIdentityForComp m x = (comp (mappend mempty m)) x == comp m x

instance Show (Comp String) where
  show _ = "Comp String"

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
    frequency [(1, Failuure <$> arbitrary), (1, Succeess <$> arbitrary)]

-- Comp
data AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Succeess a)) <> (AccumulateRight (Succeess b)) =
    AccumulateRight (Succeess (a <> b))
  _ <> (AccumulateRight (Succeess a)) = AccumulateRight (Succeess a)
  (AccumulateRight (Succeess a)) <> _ = AccumulateRight (Succeess a)
  (AccumulateRight (Failuure a)) <> _ = AccumulateRight (Failuure a)

type AccumulateRightAssoc
   = AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    x <- arbitrary
    return $ AccumulateRight x

-- Comp
data AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Succeess a)) <> (AccumulateBoth (Succeess b)) =
    AccumulateBoth (Succeess (a <> b))
  _ <> (AccumulateBoth (Succeess a)) = AccumulateBoth (Succeess a)
  (AccumulateBoth (Succeess a)) <> _ = AccumulateBoth (Succeess a)
  (AccumulateBoth (Failuure a)) <> _ = AccumulateBoth (Failuure a)

type AccumulateBothAssoc
   = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    x <- arbitrary
    return $ AccumulateBoth x

-- Comp
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
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
  quickCheck (monoidRightIdentity :: Three String String String -> Bool)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
  quickCheck (monoidRightIdentity :: Four String String String String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssocForCombine)
  quickCheck (monoidLeftIdentityForCombine)
  quickCheck (monoidRightIdentityForCombine)
  quickCheck (semigroupAssocComp)
  quickCheck (monoidLeftIdentityForComp)
  quickCheck (monoidRightIdentityForComp)
  quickCheck (semigroupAssocMem)
  quickCheck (monoidLeftIdentityForMem)
  quickCheck (monoidRightIdentityForMem)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
