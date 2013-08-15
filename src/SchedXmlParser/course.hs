module Main where

import Control.Applicative
import Data.Maybe
import Text.Read

import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Text.XML.Light
import Text.XML.Light.Types
import Text.XML.Light.Input
import Text.XML.Light.Proc

data DepartmentDef = DepartmentDef { deptDefName :: String
                                   , deptDefCode :: String }
                   deriving (Show, Eq)

data Dept = Dept { deptRef :: String
                 , courses :: [Course] }
            deriving (Show, Eq)

data Semester = Semester { semesterYear :: String
                         , semesterName :: String
                         , semesterCode :: String }
                deriving (Show, Eq)

data Course = Course { courseNo :: String
                     , sections :: [Section] }
              deriving (Show, Eq)

data Section = Section { sectionEndTime :: String
                       , sectionStartTime :: String
                       , sectionEnrollCount :: Int
                       , sectionCredits :: Int
                       , sectionNo :: String
                       , sectionSeats :: Int
                       , sectionSeatsLimit :: Int
                       , sectionCampus :: String
                       , sectionTitle :: String
                       , sectionDays :: String
                       , sectionLocation :: String
                       , sectionType :: String
                       , sectionCrn :: String
                       , sectionInstructors :: [String] }
               deriving (Show, Eq)

readDocument fp = parseXMLDoc <$> TI.readFile fp

parseDept :: Element -> Maybe Dept
parseDept elem = do
  ref <- attrNamed "dept-ref" elem
  let courses = parseCourses elem
  return $ Dept ref courses

parseCourses :: Element -> [Course]
parseCourses = parseSome "course" parseCourse

parseCourse :: Element -> Maybe Course
parseCourse elem = do
  no <- attrNamed "course-no" elem
  let sections = parseSections elem
  return $ Course no sections

parseSection :: Element -> Maybe Section
parseSection elem = do
  endTime     <- attrNamed "end-time" elem
  startTime   <- attrNamed "start-time" elem
  enrollCount <- attrNamed "enroll-count" elem >>= readMaybe
  credits     <- attrNamed "credits" elem >>= readMaybe
  sectNo      <- attrNamed "sect-no" elem
  seats       <- attrNamed "seats" elem >>= readMaybe
  seatsLimit  <- attrNamed "seats-limit" elem >>= readMaybe
  campus      <- attrNamed "campus" elem
  title       <- attrNamed "title" elem
  days        <- attrNamed "days" elem
  location    <- attrNamed "location" elem
  sectType    <- attrNamed "sect-type" elem
  crn         <- attrNamed "crn" elem
  let instructors = map strContent $ findChildren (named "instructor") elem
  return $ Section { sectionEndTime = endTime
                   , sectionStartTime = startTime
                   , sectionEnrollCount = enrollCount
                   , sectionCredits = credits
                   , sectionNo = sectNo
                   , sectionSeats = seats
                   , sectionSeatsLimit = seatsLimit
                   , sectionCampus = campus
                   , sectionTitle = title
                   , sectionDays = days
                   , sectionLocation = location
                   , sectionType = sectType
                   , sectionCrn = crn
                   , sectionInstructors = instructors }

parseSections = parseSome "section" parseSection

parseSemester :: Element -> Maybe Semester
parseSemester elem = do
  year <- attrNamed "acad-year" elem
  name <- attrNamed "sem-name"  elem
  code <- attrNamed "sem-code"  elem
  return $ Semester year name code

parseSemesters :: Element -> [Semester]
parseSemesters = parseSome "semester" parseSemester

parseDepartment :: Element -> Maybe DepartmentDef
parseDepartment elem = do
  let name = strContent elem
  code <- attrNamed "dept-code" elem
  return $ DepartmentDef name code

parseDepartments :: Element -> [DepartmentDef]
parseDepartments = parseSome "dept-def" parseDepartment

parseSome :: String -> (Element -> Maybe a) -> Element -> [a]
parseSome name parser doc = mapMaybe parser $ findChildren (named name) doc

attrNamed :: String -> Element -> Maybe String
attrNamed name elem = findAttr (named name) elem

named :: String -> QName
named s = blank_name { qName = s }
