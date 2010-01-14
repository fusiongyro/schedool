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

overlaps :: Interval -> Interval -> Bool
overlaps = undefined
