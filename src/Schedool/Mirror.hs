-- | Implements downloading and caching a local copy of the Banweb site and
--   its data. Essentially provides two functions which provide access to the
--   cached data (which will recreate it if necessary) and a function to
--   manually force a recreation.
module Schedool.Mirror ( openDepartmentData
                       , openSectionData
                       , fetch)
    where

import Schedool.Cache
import Schedool.Section
      
import Data.Char

import Data.Maybe

import Network.Browser
import Network.HTTP
import Network.URI

departmentsCache :: String
departmentsCache = "departments.html"

-- | Provides the department HTML data, whether locally or after downloading a
--   copy from the website.
openDepartmentData :: IO String
openDepartmentData = tryCache departmentsCache fetchDepartments

-- | This is the URI for the departments list dropdown.
departmentsUri :: String
departmentsUri = "http://banweb7.nmt.edu/pls/PROD/hwzkcrof.p_uncgslctcrsoff"

-- | Simple wrapper to get the content at a particular URL
fetch :: String -> IO String
fetch uri = simpleHTTP (getRequest uri) >>= getResponseBody

fetchDepartments :: IO String
fetchDepartments = fetch departmentsUri

courseUri :: String
courseUri = "http://banweb7.nmt.edu/pls/PROD/hwzkcrof.P_UncgSrchCrsOff"

type Year = Integer
data Season = Summer | Fall | Spring
data Term = Term Season Year

encodeTerm :: Term -> String
encodeTerm (Term Spring y) = show y ++ "30"
encodeTerm (Term Summer y) = show (y + 1) ++ "10"
encodeTerm (Term Fall y) = show (y + 1) ++ "20"

makeDeptRequest :: Department -> Request_String
makeDeptRequest = makeTermDeptRequest (Term Spring 2012)

makeTermDeptRequest :: Term -> Department -> Request_String
makeTermDeptRequest term (Dept _ code) =
    formToRequest $ Form POST (fromJust $ parseURI courseUri)
    -- FIXME: bad hardcoded constant! look this up in the submitted form
                      [("p_term", encodeTerm term), ("p_subj", code)]

deptToFilename :: Department -> String
deptToFilename (Dept _ code) = map toLower code ++ ".html"

fetchDepartment :: Department -> IO String
fetchDepartment dept = do
  -- send the request
  res <- simpleHTTP $ makeDeptRequest dept
  -- get the response body
  getResponseBody res

-- | Provides the section HTML data for a given department, from a local cache
--   or directly from the website.
openSectionData :: Department -> IO String
openSectionData dept = tryCache (deptToFilename dept) (fetchDepartment dept)
