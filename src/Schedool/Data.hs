module Schedool.Data (getDepartments
                     ,getSections) 
    where 

import Schedool.Cache
import Schedool.Mirror
import Schedool.Parse
import Schedool.Section

import Control.Monad

-- | The current list of departments. Uses two levels of caching.
getDepartments :: IO [Department]
getDepartments = tryCache "departments.hs" readDepartments

-- | The current list of Departments.
readDepartments :: IO [Department]
readDepartments = openDepartmentData >>= return . parseDepartments

-- | The curretn list of sections. Uses two levels of caching.
getSections :: IO [Section]
getSections = tryCache "sections.hs" readSections

-- | The current list of Sections.
readSections :: IO [Section]
readSections = do
  -- get all the departments
  depts <- readDepartments
  -- get each department's sections and concatenate all of them into one big list
  liftM concat $ forM depts readSection

readSection :: Department -> IO [Section]
readSection dept = do
  -- read the section HTML
  sectionHTML <- openSectionData dept
  -- pass it off to the parser
  return $ parseSections sectionHTML