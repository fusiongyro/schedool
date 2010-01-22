module Schedool.Mirror (openDepartmentData,
                        openSections,
                        recreate) where

openDepartmentData :: IO String
openSections :: Department -> IO String
recreate :: IO Bool
