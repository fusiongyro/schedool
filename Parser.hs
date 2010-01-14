module Parser where

import Data.Maybe
import Text.HTML.TagSoup
import Text.Regex.Posix
import Time

parseWeekday :: String -> [Weekday]
parseWeekday = catMaybes . map charToWeekday

parseInterval :: String -> (Time, Time)
parseInterval s = ((h1, m1), (h2, m2)) where
    [h1, m1, h2, m2] = map read $ s =~ "([0-9][0-9])"

-- tags >>= return . parseClass . handle . partitions (~== "<TR>")

--[TagOpen "TR" [],TagText "\n",TagOpen "TD" [],TagText "31941",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText "CSE 113-01",TagClose "TD",TagText "\n",TagOpen "TD" [("align","center")],TagText "M",TagClose "TD",TagText "\n",TagOpen "TD" [("align","center")],TagText " M W F ",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText "1300-1350",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText "CRAMER 203",TagClose "TD",TagText "\n",TagOpen "TD" [("align","center")],TagText "4",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText "Intro Computer Sci & Program",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText " ",TagClose "TD",TagText "\n",TagOpen "TD" [("align","right")],TagText "37",TagClose "TD",TagText "\n",TagOpen "TD" [("align","right")],TagText "55",TagClose "TD",TagText "\n",TagOpen "TD" [("align","right")],TagText "18",TagClose "TD",TagText "\n",TagClose "TR",TagText "\n"],[TagOpen "TR" [],TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [("align","center")],TagText "       ",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [],TagText " ",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [],TagText " ",TagClose "TD",TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [],TagText "\n",TagOpen "TD" [],TagText "\n",TagClose "TR",TagText "\n"]

tagsToStringList :: [Tag] -> [String]
tagsToStringList (TagClose "TR" : xs) = []
tagsToStringList [] = []
tagsToStringList (TagOpen "TD" [] : TagText txt : TagClose "TD" : xs) = txt : tagsToStringList xs
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
