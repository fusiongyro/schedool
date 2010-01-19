module Main where

import Fetch

main = do
  (depts, sections) <- fetchEverything
  putStrLn $ "Fetched " ++ (show $ length $ depts) ++ " departments and " ++ (show $ length $ sections) ++ " total sections."
  