module Schedool.Cache (tryCache)
    where

import System.Directory
import System.FilePath

cacheDirectory :: FilePath
cacheDirectory = "cache"

-- | The idea here is if we have a cache with this key, we look it up and
--   return it. Otherwise, we execute the supplied IO action and cache the
--   result before returning it.
tryCache :: (Read a, Show a) => String -> IO a -> IO a
tryCache key recalc = do
  -- first, make sure we have the cache directory
  createDirectoryIfMissing True cacheDirectory
  -- second, test to see if we have the cache file for this cache key
  let filePath = (cacheDirectory </> key)
  exists <- doesFileExist filePath
  -- if it exists, return that content
  if exists
     then readFile filePath >>= return . read
     else do
           -- since it doesn't, we execute the IO action and cache that
           content <- recalc
           writeFile filePath (show content)
           return content
