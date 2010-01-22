module Schedool.Cache (cacheDepartments,
                       cacheSections,
                       hasCache,
                       readCachedDepartments,
                       readCachedSections)
    where
      
cacheDepartments      :: [Department] -> IO ()
cacheSections         :: [Section]    -> IO ()

hasCache              :: IO Bool
readCachedDepartments :: IO [Department]
readCachedSections    :: IO [Section]