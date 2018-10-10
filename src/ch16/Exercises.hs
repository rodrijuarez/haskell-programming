{-# LANGUAGE FlexibleInstances #-}

module Exercises where

a = (+ 1) <$> read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = (* 2) . (\x -> x - 2)

d = ((return '1' ++) . show) . (\x -> [x,1 .. 3])

--Prelude> e
--3693
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = read . ("123" ++) <$> show <$> ioi
  in (* 3) <$> changed

-- Rearrange
data Sum b a
  = First a
  | Second b

instance Functor (Sum a) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b
  = DeepBlue a
             c
  | Something b

instance Functor (Company a c) where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a
  = L a
      b
      a
  | R b
      a
      b
  deriving (Eq, Show)

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write your functors!!!
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ (Finance) = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b =
  K a
  deriving (Show)

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

-- should remind you of an
-- -- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ f <$> fa

data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)
  deriving (Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga $ f <$> gt

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show)

instance Functor (List) where
  fmap _ (Nil) = Nil
  fmap f (Cons a b) = Cons (f a) (f <$> b)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show)

instance Functor (GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats a b c) = MoreGoats (f <$> a) (f <$> b) (f <$> c)

data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Functor (TalkToMe) where
  fmap _ (Halt) = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read f') = Read (f . f')
