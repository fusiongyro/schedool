module Schedool.Data (getDepartments
                     ,getSections) 
    where 

import Schedool.Mirror
import Schedool.Parse
import Schedool.Section

import Control.Monad

-- local caching is no longer implemented.

-- | The current list of Departments.
getDepartments :: IO [Department]
getDepartments = openDepartmentData >>= return . parseDepartments

-- | The current list of Sections.
getSections :: IO [Section]
getSections = do
  -- get all the departments
  depts <- getDepartments
  -- get each department's sections and concatenate all of them into one big list
  liftM concat $ forM depts getSection

getSection :: Department -> IO [Section]
getSection dept = do
  -- get the section HTML
  sectionHTML <- openSectionData dept
  -- pass it off to the parser
  return $ parseSections sectionHTML