module Main where

import Network.Browser
import Network.HTTP
import Network.URI
import Data.Maybe

url = "http://clanspum.net/~fusion/testhttp.php"

test = do
  (uri, rsp) <- Network.Browser.browse $ do
                 setAllowRedirects True
                 request $ formToRequest $
                         Form POST (fromJust $ parseURI url)
                              [("hello", "world")]
  return rsp

