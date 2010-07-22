module Schedool.Data (getDepartments
                     ,getSections) 
    where 

import Schedool.Cache
import Schedool.Mirror
import Schedool.Parse
import Schedool.Section

import Control.Applicative
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
readSections = readDepartments >>= (\x -> concat <$> mapM readSection x)

readSection :: Department -> IO [Section]
readSection dept = parseSections <$> openSectionData dept
