module DataConstructors where

data PugType =
  PugData
  deriving (Show)

data HuskyType a =
  HuskyData
  deriving (Show)

data DogueDeBordeaux doge =
  DogueDeBordeaux doge
  deriving (Show)

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

-- no witness to the contrary ^
-- -- This will work because the value 10 agrees -- with the type variable being bound to Int myDoge :: DogueDeBordeaux Int
-- myDoge = DogueDeBordeaux 10
-- -- This will not work because 10
-- -- cannot be reconciled with the
-- -- type variable being bound to String
badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux "asdasd"

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)
-- type constructor awaiting an argument
