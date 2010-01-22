module Schedool.Schedule where

import Schedool.Time
import Schedool.Section
import Schedool.Fetch
import Schedool.Utility
import Schedool.Overlap

import Control.Monad

type Schedule = [Section]

main = do
  (_, sects) <- reloadCached
  let overlappingStuff = [(l,r, l `overlaps` r) | (l,r) <- combinations sects]
  forM_ overlappingStuff $ \(l, r, o) -> do
    putStrLn $ (pshow l) ++ doesOrDoesNot o ++ (pshow r)
  where
      doesOrDoesNot o = (if o then " overlaps " else " does not overlap ")
