{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import System.IO
import Text.Printf

import Schedool.Time
import Schedool.Query

queryInteractively :: IO ()
queryInteractively = getQueryContext >>= forever . oneQuery

oneQuery :: Catalog -> IO ()
oneQuery cat = do
  hSetBuffering stdin LineBuffering
  putStr "> "
  hFlush stdout
  line <- getLine
  displayResults $ executeQuery cat line

displayResults :: [[Section]] -> IO ()
displayResults = mapM_ displayResult
    where
      displayResult :: [Section] -> IO ()
      displayResult sects = do
        putStrLn "-----"
        mapM_ displaySection sects
        putStrLn ""
                 
      displaySection :: Section -> IO ()
      displaySection s = printf "%-4s %s-%d (%d) from %d:%02d--%d:%02d %s at %s (%s)\n" dept course section crn hour min stopHour stopMin on at instructor
          where
            (Section
             { section   = section,
               crn = crn,
               sectionOf = (ClassInfo { department = dept, course = course}),
               location  = (LocationInfo { campus = campus, room = at }),
               schedule  = (ScheduleInfo { days = days, start = (hour, min), stop = (stopHour, stopMin) }), instructor = Just instructor }) = s
            on      = map weekdayToChar days

main :: IO ()
main = queryInteractively