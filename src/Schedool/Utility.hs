-- | A collection of helpful combinators that don't really belong to any other
--   part of the app.
module Schedool.Utility where

-- | Returns every pairwise combination of the elements of the list.
combinations :: [a] -> [(a, a)]
combinations []     = []
combinations (x:xs) = distribute x xs ++ combinations xs
    where
      distribute x (y:ys) = (x,y) : distribute x ys
      distribute _ []     = []
