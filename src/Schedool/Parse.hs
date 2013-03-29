module Schedool.Parse (parseDepartments
                      ,parseSections)
    where

import Schedool.Section
import Schedool.Time
import Schedool.Utility (strNormal)

import Data.Array
import Control.Exception
import Control.Monad
import Data.Maybe
import Text.HTML.TagSoup
import Text.Regex.Posix

parseDepartments :: String -> [Department]
parseDepartments = parseDepts . (!! 1) . sections (~== "<SELECT>") . parseTags

parseSections :: String -> [Section]
parseSections s = mapMaybe parseRow $ breakRows $ parseTags s

parseDepts :: [Tag String] -> [Department]
parseDepts (TagOpen "SELECT" _ : xs) = parseDepts xs
parseDepts (TagOpen "OPTION" [("VALUE", code)] :
            TagText name : xs) = Dept (strNormal name) code : parseDepts xs
parseDepts (TagClose "SELECT" : _) = []
parseDepts (_ : xs) = parseDepts xs
parseDepts [] = []

(!?) :: (Ix ix) => Array ix v -> ix -> Maybe v
arr !? i = if i > min' && i < max' then Just (arr ! i) else Nothing
    where
      (min', max') = bounds arr

parseWeekdays :: String -> [Weekday]
parseWeekdays = mapMaybe charToWeekday

parseInterval :: String -> Maybe (Time, Time)
parseInterval s = case map (read . head) (s =~ "([0-9][0-9])") of
                    [h1, m1, h2, m2] -> Just ((h1, m1), (h2, m2))
                    _                -> Nothing

readInteger :: String -> Maybe Integer
readInteger x = case x =~ "^[0-9]+$" of
                 [[s]] -> Just $ read s
                 _     -> Nothing

strip :: String -> String
strip = unwords . words

rowToArray :: [Tag String] -> Array Int String
rowToArray tags = listArray (0, length l - 1) l
    where
      l = rowToArray' tags
      rowToArray' :: [Tag String] -> [String]
      rowToArray' (TagClose "TR" : _) = []
      rowToArray' [] = []
      rowToArray' (TagOpen "TD" _ : TagText txt : TagClose "TD" : xs) = strip txt : rowToArray' xs
      rowToArray' (_:xs) = rowToArray' xs

breakRows :: [Tag String] -> [[Tag String]]
breakRows = partitions (~== "<TR>")

parseCourseInfo :: Array Int String -> Maybe (String, String, Integer)
parseCourseInfo a = case concat $ (a ! 1) =~ "([^ ]+) ([0-9]+)-([0-9]+)" of
                         [_, dept, c, sect1] -> Just (dept, c, read sect1)
                         _ -> Nothing

parseClassInfo :: Array Int String -> (String, String, Integer) -> Maybe ClassInfo
parseClassInfo a (dept, c, _) = Just (ClassInfo dept c credits')
    where
      credits' = read $ a ! 6

parseLocationInfo :: Array Int String -> Maybe LocationInfo
parseLocationInfo a = Just (LocationInfo campus' room' limit')
    where
      campus' = a ! 2
      room'   = a ! 5
      limit'  = a !? 10 >>= readInteger

parseScheduleInfo :: Array Int String -> Maybe ScheduleInfo
parseScheduleInfo a = case parseInterval (a ! 4) of
                        Just (start', stop') -> Just (ScheduleInfo (parseWeekdays (a ! 3)) start' stop')
                        Nothing -> Nothing

-- I still do not like this function, but this is much better
parseSection  :: Array Int String -> Maybe Section
parseSection a = do
  let (_, max') = bounds a
  guard (max' >= 6)
  coi@(_,_,section') <- parseCourseInfo a
  ci <- parseClassInfo a coi
  li <- parseLocationInfo a
  si <- parseScheduleInfo a
  let instructor' = a !? 8 >>= (\x -> if x == "" then Nothing else Just x)
      enrolled'   = a !? 9 >>= readInteger
  return (Section (read $ a ! 0) section' ci li si instructor' enrolled')

parseRow :: [Tag String] -> Maybe Section
parseRow = parseSection . rowToArray

-- testing crap
{-
readSections :: FilePath -> IO [Section]
readSections f = parseSections <$> readFile f

rowN :: Int -> IO [Tag String]
rowN n = readFile "cs.html" >>= return . head . drop n . breakRows . parseTags
-}
