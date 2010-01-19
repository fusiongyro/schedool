module Parse -- (parseSections, readSections)
    where

import Section

import Array
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List
import Text.HTML.TagSoup
import Text.Regex.Posix
import Time

(!?) :: (Ix ix) => Array ix v -> ix -> Maybe v
arr !? i = if i > min && i < max then Just (arr ! i) else Nothing
    where
      (min, max) = bounds arr

parseWeekdays :: String -> [Weekday]
parseWeekdays = catMaybes . map charToWeekday

parseInterval :: String -> Maybe (Time, Time)
parseInterval s = case (map read (s =~ "([0-9][0-9])")) of
                    [h1, m1, h2, m2] -> Just ((h1, m1), (h2, m2))
                    _                -> Nothing

readInteger :: String -> Maybe Integer
readInteger x = case x =~ "^[0-9]+$" of
                 [s] -> Just $ read s
                 _ -> Nothing

strip :: String -> String
strip = unwords . words

rowToArray :: [Tag] -> Array Int String
rowToArray tags = listArray (0, length l - 1) l
    where
      l = rowToArray' tags
      rowToArray' :: [Tag] -> [String]
      rowToArray' (TagClose "TR" : xs) = []
      rowToArray' [] = []
      rowToArray' (TagOpen "TD" _ : TagText txt : TagClose "TD" : xs) = strip txt : rowToArray' xs
      rowToArray' (_:xs) = rowToArray' xs

breakRows :: [Tag] -> [[Tag]]
breakRows = partitions (~== "<TR>")

parseCourseInfo :: Array Int String -> Maybe (Department, String, Integer)
parseCourseInfo a = case (a ! 1) =~ "([^ ]+) ([0-9]+)-([0-9]+)" of
			 [dept1, course, sect1] -> Just (read dept1, course, read sect1)
			 _ -> Nothing

parseClassInfo :: Array Int String -> (Department, String, Integer) -> Maybe ClassInfo
parseClassInfo a (dept, course, sect) = Just (ClassInfo crn dept course sect credits)
    where
      crn = read $ a ! 0
      course = a ! 1
      credits = read $ a ! 6

parseLocationInfo :: Array Int String -> Maybe LocationInfo
parseLocationInfo a = Just (LocationInfo campus room limit)
    where
      campus = a ! 2
      room =  a ! 5
      limit = a !? 10 >>= readInteger

parseScheduleInfo :: Array Int String -> Maybe ScheduleInfo
parseScheduleInfo a = case parseInterval (a ! 4) of
                        Just (start, stop) -> Just (ScheduleInfo (parseWeekdays (a ! 3)) start stop)
                        Nothing -> Nothing

-- I still do not like this function, but this is much better
parseSection  :: Array Int String -> Maybe Section
parseSection a = do
  let (_, max) = bounds a
  guard (max >= 6)
  coi <- parseCourseInfo a
  ci <- parseClassInfo a coi
  li <- parseLocationInfo a
  si <- parseScheduleInfo a
  let instructor = a !? 8 >>= (\x -> if x == "" then Nothing else Just x)
      enrolled = a !? 9 >>= readInteger
  return (Section ci li si instructor enrolled)

parseRow :: [Tag] -> Maybe Section
parseRow = parseSection . rowToArray

parseSections :: String -> [Section]
parseSections s = catMaybes $ map parseRow $ breakRows $ parseTags s

readSections :: FilePath -> IO [Section]
readSections f = readFile f >>= return . parseSections

-- testing crap
rowN :: Int -> IO [Tag]
rowN n = readFile "cs.html" >>= return . head . drop n . breakRows . parseTags
