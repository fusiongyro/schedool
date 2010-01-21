{-# LANGUAGE TypeSynonymInstances #-}

-- | Time and date related types and functions.
module Time (Hour,
             Minute,
             Weekday(..),
             Time,
             Interval,
             toInterval,
             charToWeekday,
             weekdayToChar) where

import Overlap

-- | An hour is simply an integer.
type Hour   = Integer -- [0..23]
-- | A minute is also simply an integer.
type Minute = Integer -- [0..59]

-- | A simple day-of-the-week type.
data Weekday  = Monday
              | Tuesday
              | Wednesday
              | Thursday
              | Friday
              | Saturday
              | Sunday
                deriving (Show, Eq, Ord, Read)

-- | This and its opposite 'weekdayToChar' are convenient for the shorthand
--   notation used in Banweb.
charToWeekday :: Char -> Maybe Weekday
charToWeekday 'M' = Just Monday
charToWeekday 'T' = Just Tuesday
charToWeekday 'W' = Just Wednesday
charToWeekday 'R' = Just Thursday
charToWeekday 'F' = Just Friday
charToWeekday 'S' = Just Saturday
charToWeekday 'U' = Just Sunday
charToWeekday _   = Nothing

-- | Opposite of 'charToWeekday': given a day, return the corresponding
--   shorthand character.
weekdayToChar :: Weekday -> Char
weekdayToChar Monday    = 'M'
weekdayToChar Tuesday   = 'T'
weekdayToChar Wednesday = 'W'
weekdayToChar Thursday  = 'R'
weekdayToChar Friday    = 'F'
weekdayToChar Saturday  = 'S'
weekdayToChar Sunday    = 'U'

-- | A type for 4:20
type Time     = (Hour, Minute)

-- | Time intervals. Notice you cannot represent intervals spanning more
--   than a day.
type Interval = (Time, Time, Weekday)

-- | Detect overlapping time intervals.
instance Overlappable Interval where
  (s1, t1, d1) `overlaps` (s2, t2, d2) | d1 == d2    = s2 < t1 && s1 < t2
                                     | otherwise  = False

-- | Converts a tuple of (Time, Time, Weekday) to our Interval type.
toInterval :: (Time, Time, Weekday) -> Interval
toInterval = id
