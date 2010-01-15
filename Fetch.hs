module Fetch where

import Parser

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

getDepartments :: IO [Department]
getDepartments = do
  html <- readFile "courses.html"
  return $ parseDepts $ head $ sections (~== "<SELECT name=p_subj>") $ parseTags html

courseUri :: String
courseUri = "https://banweb7.nmt.edu/pls/PROD/hwzkcrof.P_UncgSrchCrsOff"

makeRequest :: Department -> Request_String
makeRequest (Dept _ code) =
    formToRequest $ Form POST (fromJust $ parseURI courseUri)
                      [("p_term", "201030"), ("p_subj", code)]


loadDepartment :: Department -> IO [Class]
loadDepartment dept = do
  res <- simpleHTTP $ makeRequest dept
  content <- getResponseBody res
  return $ parseClasses content

fetchAllClasses :: IO [Class]
fetchAllClasses = do
  depts <- getDepartments
  classes <- mapM loadDepartment depts
  return $ concat classes