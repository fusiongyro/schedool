module Schedool.Section (Section(..)
                        ,ClassInfo(..)
                        ,LocationInfo(..)
                        ,ScheduleInfo(..)

                        ,Name
                        ,Code
                        ,Department(..)
                        ,toInterval
                        ,pshow)
    where

import Schedool.Overlap
import Schedool.Time hiding (toInterval)

-- | The class you register for is actually a section, that is, an instance of
--   a particular course being taught at a particular time and place by a
--   particular professor, with so many students enrolled.
data Section = Section {
    crn         :: Integer,
    section     :: Integer,
    sectionOf   :: ClassInfo,
    location    :: LocationInfo,
    schedule    :: ScheduleInfo,
    instructor  :: Maybe String,
    enrolled    :: Maybe Integer }
    deriving (Show, Eq, Read)

-- | The class info is the platonic ideal of the class; the material, the
--   department it belongs to and how many credit hours it is. If we had more
--   information, such as a description, it should go here.
data ClassInfo = ClassInfo {
    department  :: String,
    course      :: String,
    credits     :: Integer }
    deriving (Show, Eq, Read)

-- | Represents the campus, the room and the number this room can seat (a
--   minor violation of normalization, due to the nature of our data format).
data LocationInfo = LocationInfo {
    campus      :: String,
    room        :: String,
    limit       :: Maybe Integer }
    deriving (Show, Eq, Read)

-- | The calendar data which interests me about this particular class: when it
--   meets on which days. Note that this data structure implicitly prevents us
--   from having to face the restriction of 'Time.Interval' that they cannot
--   handle intervals that span the barriers between days. (Tech doesn't teach
--   at midnight).
data ScheduleInfo = ScheduleInfo {
    days        :: [Weekday],
    start       :: Time,
    stop        :: Time }
    deriving (Show, Eq, Read)

type Name = String
type Code = String

-- | The department is broken down into the conjunction of its name and the
--   code which represents it in Banweb or the catalog.
data Department = Dept Name Code
                  deriving (Show, Eq, Read)

-- | Converts our ScheduleInfo into a list of intervals. Note that the result
--   can be passed directly to 'Overlap.overlaps'.
toInterval :: ScheduleInfo -> [Interval]
toInterval (ScheduleInfo days start stop) = map (\x -> (start,stop,x)) days

-- | ScheduleInfo is itself overlappable.
instance Overlappable ScheduleInfo where
  s1 `overlaps` s2 = (toInterval s1) `overlaps` (toInterval s2)

-- | Section is itself overlappable, as it aggregates ScheduleInfo.
instance Overlappable Section where
  s1 `overlaps` s2 = (schedule s1) `overlaps` (schedule s2)

-- | A handy display routine used to much objection in modules that must
--   format data.
pshow :: Section -> String
pshow s = showClassInfo (sectionOf s) ++ " (" ++ (showSchedInfo (schedule s)) ++ ")"
    where
      showClassInfo (ClassInfo dept course _) = dept ++ " " ++ course ++ "-" ++ (show (section s))
      showSchedInfo (ScheduleInfo days start stop)   = (showT start) ++ "-" ++ (showT stop) ++ " " ++ (weekdays days)
      weekdays = map weekdayToChar
      showT (hour,minute)     = (show hour) ++ ":" ++ (show minute)
