module Section where

import Time hiding (toInterval)

data ClassInfo = ClassInfo { crn         :: Integer,
			                 department  :: String,
                             course      :: String,
			                 section     :: Integer,
                             credits     :: Integer }
                 deriving (Show, Eq, Read)

data LocationInfo = LocationInfo { campus  :: String,
                                   room    :: String,
                                   limit   :: Maybe Integer }
                    deriving (Show, Eq, Read)

data ScheduleInfo = ScheduleInfo { days        :: [Weekday],
                                   start       :: Time,
                                   stop        :: Time }
                    deriving (Show, Eq, Read)

data Section = Section { sectionOf   :: ClassInfo,
                         location    :: LocationInfo,
                         schedule    :: ScheduleInfo,
                         instructor  :: Maybe String,
                         enrolled    :: Maybe Integer }
               deriving (Show, Eq, Read)

type Name = String
type Code = String

data Department = Dept Name Code
                  deriving (Show, Eq, Read)

toInterval :: ScheduleInfo -> [Interval]
toInterval (ScheduleInfo days start stop) = map (\x -> (start,stop,x)) days

instance Overlappable ScheduleInfo where
  s1 `overlaps` s2 = (toInterval s1) `overlaps` (toInterval s2)

instance Overlappable Section where
  s1 `overlaps` s2 = (schedule s1) `overlaps` (schedule s2)

pshow :: Section -> String
pshow s = showClassInfo (sectionOf s) ++ " (" ++ (showSchedInfo (schedule s)) ++ ")"
    where
      showClassInfo (ClassInfo _ dept course sect _) = dept ++ " " ++ course ++ "-" ++ (show sect)
      showSchedInfo (ScheduleInfo days start stop)   = (showT start) ++ "-" ++ (showT stop) ++ " " ++ (weekdays days)
      weekdays (Monday:xs)    = 'M' : weekdays xs
      weekdays (Tuesday:xs)   = 'T' : weekdays xs
      weekdays (Wednesday:xs) = 'W' : weekdays xs
      weekdays (Thursday:xs)  = 'R' : weekdays xs
      weekdays (Friday:xs)    = 'F' : weekdays xs
      weekdays (Saturday:xs)  = 'S' : weekdays xs
      weekdays (Sunday:xs)    = 'U' : weekdays xs
      weekdays []             = []
      showT (hour,minute)     = (show hour) ++ ":" ++ (show minute)
