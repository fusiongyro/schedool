module Parser where

import Data.Maybe
import Text.HTML.TagSoup
import Time

parseWeekday :: String -> [Weekday]
parseWeekday = catMaybes . map charToWeekday
