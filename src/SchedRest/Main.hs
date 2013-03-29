{-# LANGUAGE ViewPatterns #-}

module SchedRest.Main where

import Schedool.Query

import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as L
import Text.JSON
import Text.JSON.Generic
import Happstack.Server
import Happstack.Server.SimpleHTTP

main :: IO ()
main = simpleHTTP nullConf myApp

myApp :: ServerPart Response
myApp = do
	query <- L.unpack <$> lookText "query"
	queryOutput <- liftIO $ runQuery query
	setHeaderM "Content-Type" "application/json"
	ok $ toResponse $ encodeJSON queryOutput
