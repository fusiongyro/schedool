module Parser where

import Data.Maybe
import Data.List
import Text.HTML.TagSoup
import Text.Regex.Posix
import Time

parseWeekday :: String -> [Weekday]
parseWeekday = catMaybes . map charToWeekday

parseInterval :: String -> (Time, Time)
parseInterval s = ((h1, m1), (h2, m2)) where
    [h1, m1, h2, m2] = map read $ s =~ "([0-9][0-9])"

tagsToStringList :: [Tag] -> [String]
tagsToStringList (TagClose "TR" : xs) = []
tagsToStringList [] = []
tagsToStringList (TagOpen "TD" _ : TagText txt : TagClose "TD" : xs) = txt : tagsToStringList xs
tagsToStringList (_:xs) = tagsToStringList xs

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
parseClass (crn : course : campus : weekdays : startStop : location : creditHours : title : rest) =
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
        where
          (start, stop) = parseInterval startStop
          improveClass c [] = c
          improveClass c [i] = c { instructor = Just i }
          improveClass c [i, s] = c { instructor = Just i, seats = Just (read s) }
          improveClass c [i, s, l] = c { instructor = Just i, seats = Just (read s), limit = Just (read l) }
          improveClass c [i, s, l, e] = c { instructor = Just i, seats = Just (read s), limit = Just (read l), enrolled = Just (read e)}
parseClass _ = Nothing

-- {-
readClasses :: FilePath -> IO [Class]
readClasses file = do
  content <- readFile file
  let tags = parseTags content
  let parts = partitions (~== "<TR>") tags
  let maybeClasses = map (parseClass . tagsToStringList) parts
  return $ catMaybes maybeClasses
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