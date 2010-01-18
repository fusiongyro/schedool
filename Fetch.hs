module Fetch where

import Parser
import Class

import Data.Char
import Data.Maybe

import Network.Browser
import Network.HTTP
import Network.URI

import Text.HTML.TagSoup


type Name = String
type Code = String

data Department = Dept Name Code
                deriving (Show, Eq, Read)

strNormal :: String -> String
strNormal = unwords . words

parseDepts (TagOpen "SELECT" _ : xs) = parseDepts xs
parseDepts (TagOpen "OPTION" [("value", code)] :
            TagText name :
            TagClose "OPTION" : xs) = (Dept (strNormal name) code) : parseDepts xs
parseDepts (TagClose "SELECT" : xs) = []
parseDepts (x : xs) = parseDepts xs
parseDepts [] = []


departmentsUri :: String
departmentsUri = "https://banweb7.nmt.edu/pls/PROD/hwzkcrof.p_uncgslctcrsoff"

fetchDepartments :: IO [Department]
fetchDepartments = do
  resp <- simpleHTTP $ getRequest departmentsUri
  body <- getResponseBody resp
  return $ parseDepartments body

getDepartments :: IO [Department]
getDepartments = do
  html <- readFile "courses.html"
  return $ parseDepartments html

parseDepartments :: String -> [Department]
parseDepartments = parseDepts . head . sections (~== "<SELECT name=p_subj>") . parseTags

courseUri :: String
courseUri = "https://banweb7.nmt.edu/pls/PROD/hwzkcrof.P_UncgSrchCrsOff"

makeRequest :: Department -> Request_String
makeRequest (Dept _ code) =
    formToRequest $ Form POST (fromJust $ parseURI courseUri)
                      [("p_term", "201030"), ("p_subj", code)]

deptToFilename :: Department -> String
deptToFilename (Dept name _) = "cache/" ++ (map toLower name) ++ ".html"

loadDepartment :: Department -> IO [Section]
loadDepartment dept@(Dept name code) = do
  putStr $ "Fetching " ++ name ++ "..."
  -- send the request
  res <- simpleHTTP $ makeRequest dept
  -- get the response body
  content <- getResponseBody res
  -- save a cache copy here
  writeFile (deptToFilename dept) content
  putStrLn "done"
  -- return the content
  return $ parseSections content

fetchAllClasses :: IO [Section]
fetchAllClasses = do
  depts <- getDepartments
  classes <- mapM loadDepartment depts
  return $ concat classes