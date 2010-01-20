module Schedule where

import Time
import Section
import Fetch

import Control.Monad

main = do
  (_, sects) <- reloadCached
  let overlappingStuff = [(l,r, l `overlaps` r) | (l,r) <- combinations sects]
  forM_ overlappingStuff $ \(l, r, o) -> do
    putStrLn $ (pshow l) ++ doesOrDoesNot o ++ (pshow r)
  where
      doesOrDoesNot o = (if o then " overlaps " else " does not overlap ")
