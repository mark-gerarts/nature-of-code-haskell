module NatureOfCode.Util (mapRange) where

-- |Maps a value within range A to the same value relative to range B
-- e.g. `mapRange (0,10) (0,100) 5 == 50
-- Credit: https://rosettacode.org/wiki/Map_range#Haskell.
mapRange :: Fractional a => (a, a) -> (a, a) -> a -> a
mapRange (a1, a2) (b1, b2) s = b1 + (s - a1) * (b2 - b1) / (a2 - a1)
