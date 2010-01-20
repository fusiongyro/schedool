{-# LANGUAGE TypeSynonymInstances #-}

module Time where

type Hour   = Integer -- [0..23]
type Minute = Integer -- [0..59]

data Weekday  = Monday
              | Tuesday
              | Wednesday
              | Thursday
              | Friday
              | Saturday
              | Sunday
                deriving (Show, Eq, Ord, Read)

charToWeekday :: Char -> Maybe Weekday
charToWeekday 'M' = Just Monday
charToWeekday 'T' = Just Tuesday
charToWeekday 'W' = Just Wednesday
charToWeekday 'R' = Just Thursday
charToWeekday 'F' = Just Friday
charToWeekday 'S' = Just Saturday
charToWeekday 'U' = Just Sunday
charToWeekday _   = Nothing

type Time     = (Hour, Minute)
type Interval = (Time, Time, Weekday)

hour :: Time -> Hour
hour (h, _) = h

minute :: Time -> Minute
minute (_, m) = m

start :: Interval -> Time
start (s, _, _) = s

stop :: Interval -> Time
stop (_, s, _) = s

class Overlappable a where
  overlaps :: a -> a -> Bool

instance Overlappable Interval where
  -- I halfway wonder if this should be a typeclass instead of a function
  -- overlaps :: Interval -> Interval -> Bool
  (s1, t1, d1) `overlaps` (s2, t2, d2) | d1 == d2    = s2 < t1 && s1 < t2
                                     | otherwise  = False

toInterval :: (Time, Time, Weekday) -> Interval
toInterval = id

instance (Overlappable a) => Overlappable [a] where
  overlaps left right = or [l `overlaps` r | l <- left, r <- right ]

combinations :: [a] -> [(a, a)]
combinations []     = []
combinations (x:xs) = distribute x xs ++ combinations xs
    where
      distribute x (y:ys) = (x,y) : distribute x ys
      distribute _ []     = []

noOverlaps :: (Overlappable a) => [a] -> Bool
noOverlaps xs = not (any (uncurry overlaps) (combinations xs))

t1, t2, t3, t4, t5, t6 :: Interval
t1 = (( 9,00), ( 9,30), Monday) --  9:00--9:30  M
t2 = (( 9,30), (10,15), Monday) --  9:30--10:15 M
t3 = ((10,00), (10,50), Monday) -- 10:00--10:50 M
t4 = (( 9,00), ( 9,30), Friday) --  9:00--9:30  F
t5 = (( 9,30), (10,15), Friday) --  9:30--10:15 F
t6 = ((10,00), (10,50), Friday) -- 10:00--10:50 F
