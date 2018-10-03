module Vehicles where

data Price -- (a)
      =
  Price Integer
  deriving (Eq, Show)

data Size -- (a)
      =
  Size Integer
  deriving (Eq, Show)

data Manufacturer -- (c)
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show) -- one type constructor (c)
  -- three data constructors (d), (e), and (f)

-- -- --
data Airline -- (g)
  = PapuAir
  | CatapultsR'Us -- (i)
  | TakeYourChancesUnited -- (j)
  deriving (Eq, Show) -- one type constructor (g).

-- (h)
-- three data constructors, (h), (i), and (j)
--
data Vehicle
  = Car Manufacturer
        Price -- (k) (l) [2] [3]
  | Plane Airline
          Size -- (m) [4]
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = undefined
