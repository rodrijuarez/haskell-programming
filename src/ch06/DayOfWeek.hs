module DayOfWeek where

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show, Enum, Bounded)

data Date =
  Date DayOfWeek
       Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekDay dayOfMonth) (Date weekDay' dayOfMonth') =
    weekDay == weekDay' && dayOfMonth == dayOfMonth'

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

succ' :: (Eq a, Enum a, Bounded a) => a -> a
succ' d
  | d == maxBound = minBound
  | otherwise = succ d
