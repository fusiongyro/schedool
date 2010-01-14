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
                deriving (Show, Eq, Ord)

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

-- I halfway wonder if this should be a typeclass instead of a function
overlaps :: Interval -> Interval -> Bool
overlaps (s1, t1, d1) (s2, t2, d2) | d1 == d2   = s2 < t1 && s1 < t2
                                   | otherwise  = False

