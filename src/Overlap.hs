-- | Defines a class of types that support detection of overlapping values.
module Overlap (Overlappable,
                overlaps,
                hasOverlaps,
                noOverlaps) where

import Utility

-- | The class of overlappable values
class Overlappable a where
  overlaps :: a -> a -> Bool

-- | Lists of overlappable values. If no value from one list overlaps with any
--   value from the other list, the two must not overlap.
instance (Overlappable a) => Overlappable [a] where
  overlaps left right = or [l `overlaps` r | l <- left, r <- right ]

-- | Given a list of overlappable values, determine whether or not it contains
--   overlapping values.
hasOverlaps :: (Overlappable a) => [a] -> Bool
hasOverlaps xs = any (uncurry overlaps) (combinations xs)

noOverlaps :: (Overlappable a) => [a] -> Bool
noOverlaps = not . hasOverlaps
