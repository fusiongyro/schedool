module Section where

import Time

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
