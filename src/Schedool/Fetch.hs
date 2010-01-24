module Schedool.Fetch -- (fetchEverything)
    where

import Schedool.Parse
import Schedool.Section

import Directory
import Data.Char
import Data.Maybe


import Text.HTML.TagSoup


preserveDepartments :: [Department] -> IO ()
preserveDepartments depts = writeFile "departments.hs" $ show depts

preserveSections :: [Section] -> IO ()
preserveSections sections = writeFile "sections.hs" $ show sections

reloadDepartments :: IO [Department]
reloadDepartments = readFile "courses.html" >>= return . parseDepartments

reloadCached :: IO ([Department], [Section])
reloadCached = do
  depts <- readFile "departments.hs" >>= return . read
  sects <- readFile "sections.hs" >>= return . read
  return (depts, sects)

reloadEverything :: IO ([Department], [Section])
reloadEverything = do
  putStrLn "reloading departments"
  depts    <- reloadDepartments
  preserveDepartments depts
  putStrLn "reloading cache"
  files    <- getDirectoryContents "cache" >>= return . drop 2
  sections <- mapM readSections (map ("cache/"++) files) >>= return . concat
  preserveSections sections
  return (depts, sections)

fetchEverything :: IO ([Department], [Section])
fetchEverything = do
  putStr "Fetching departments..."
  depts <- fetchDepartments
  putStrLn "done"
  preserveDepartments depts
  sections <- mapM loadDepartment depts >>= return . concat
  preserveSections sections
  return (depts, sections)

main = do
  (depts, sections) <- fetchEverything
  putStrLn $ "Fetched " ++ (show $ length $ depts) ++ " departments and " ++ (show $ length $ sections) ++ " total sections."
