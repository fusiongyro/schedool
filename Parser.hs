module Parser where

import Data.Maybe
import Data.List
import Text.HTML.TagSoup
import Text.Regex.Posix
import Time

parseWeekday :: String -> [Weekday]
parseWeekday = catMaybes . map charToWeekday

parseInterval :: String -> Maybe (Time, Time)
parseInterval s = case (map read (s =~ "([0-9][0-9])")) of
                    [h1, m1, h2, m2] -> Just ((h1, m1), (h2, m2))
                    _                -> Nothing

tagsToStringList :: [Tag] -> [String]
tagsToStringList (TagClose "TR" : xs) = []
tagsToStringList [] = []
tagsToStringList (TagOpen "TD" _ : TagText txt : TagClose "TD" : xs) = txt : tagsToStringList xs
tagsToStringList (_:xs) = tagsToStringList xs

data ClassInfo    { crn         :: Integer,
                    course      :: String,
                    credits     :: Integer }

data LocationInfo { campus      :: String,
                    location    :: String,
                    limit       :: Maybe Integer }

data ScheduleInfo { days        :: [Weekday],
                    start       :: Time,
                    stop        :: Time }

data Section      { sectionOf   :: ClassInfo,
                    location    :: LocationInfo,
                    schedule    :: ScheduleInfo,
                    instructor  :: String,
                    enrolled    :: Integer }


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

parseClass :: [String] -> Maybe Class
parseClass (_:_:_:_:sS:_) as l =
    case parseInterval sS of
      Nothing        -> Nothing
      Just startStop -> parseClass' startStop l
          where
            parseClass' (start, stop) (crn : course : campus : weekdays : _ : location : creditHours : title : rest) =
                Just $ improveClass (Class (read crn)
                                         course
                                         campus
                                         (parseWeekday weekdays)
                                         start
                                         stop
                                         location
                                         (read creditHours)
                                         title
                                         Nothing Nothing Nothing Nothing) rest
            improveClass c [] = c
            improveClass c [i] = c { instructor = Just i }
            improveClass c [i, s] = c { instructor = Just i, seats = Just (read s) }
            improveClass c [i, s, l] = c { instructor = Just i, seats = Just (read s), limit = Just (read l) }
            improveClass c [i, s, l, e] = c { instructor = Just i, seats = Just (read s), limit = Just (read l), enrolled = Just (read e)}
parseClass _ = Nothing

parseClasses :: String -> [Class]
parseClasses c = rowsToClasses $ breakRows $ parseTags c

breakRows :: [Tag] -> [[Tag]]
breakRows = partitions (~== "<TR>")

rowsToClasses :: [[Tag]] -> [Class]
rowsToClasses tags = catMaybes $ map (parseClass . tagsToStringList) tags

-- {-
readClasses :: FilePath -> IO [Class]
readClasses file = readFile file >>= return . parseClasses

-- -}
--  return concat $ catMaybes $ map parseClass $ tagsToStringList tags
--  return catMaybes $ concatMap (parseClass . tagsToStringList) parts

classesToCSV :: [Class] -> String
classesToCSV = concatMap classToCSV
    where
      classToCSV (Class crn
                      course
                      campus
                      days
                      start
                      stop
                      location
                      creditHours
                      classTitle
                      instructor
                      seats
                      limit
                      enrolled) = (intercalate "," simpleStrings) ++ "\n"
          where
            simpleStrings :: [String]
            simpleStrings = [show crn, course, campus, daysToCSV days, timeToString start, timeToString stop, location, show creditHours, classTitle, fromMaybe "" instructor, maybe "" show seats, maybe "" show limit, maybe "" show enrolled]
            daysToCSV = intercalate "," . map show
            timeToString (h,m) = show h ++ ":" ++ show m
