module Section where

import Overlap
import Time hiding (toInterval)

-- crn and section should be moved to Section
data ClassInfo = ClassInfo {
	department  :: String,
    course      :: String,
    credits     :: Integer }
    deriving (Show, Eq, Read)

data LocationInfo = LocationInfo {
    campus      :: String,
    room        :: String,
    limit       :: Maybe Integer }
    deriving (Show, Eq, Read)

data ScheduleInfo = ScheduleInfo {
    days        :: [Weekday],
    start       :: Time,
    stop        :: Time }
    deriving (Show, Eq, Read)

data Section = Section {
    crn         :: Integer,
	section     :: Integer,
    sectionOf   :: ClassInfo,
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
      showClassInfo (ClassInfo dept course _) = dept ++ " " ++ course ++ "-" ++ (show (section s))
      showSchedInfo (ScheduleInfo days start stop)   = (showT start) ++ "-" ++ (showT stop) ++ " " ++ (weekdays days)
      weekdays = map weekdayToChar
      showT (hour,minute)     = (show hour) ++ ":" ++ (show minute)
