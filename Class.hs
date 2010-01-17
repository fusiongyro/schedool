module Class where

import Time

data ClassInfo = ClassInfo { crn         :: Integer,
                             course      :: String,
                             credits     :: Integer }
                 deriving (Show, Eq, Read)

data LocationInfo = LocationInfo { campus      :: String,
                                   room    :: String,
                                   limit       :: Maybe Integer }
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

{-
data Class = Class {crn :: Integer,
                    course :: String,
                    campus :: String,
                    days :: [Weekday],
                    start :: Time,
                    stop :: Time,
                    location :: String,
                    creditHours :: Integer,
                    classTitle :: String,
                    instructor :: Maybe String,
                    seats :: Maybe Integer,
                    limit :: Maybe Integer,
                    enrolled :: Maybe Integer }
           deriving (Show, Eq)

-}