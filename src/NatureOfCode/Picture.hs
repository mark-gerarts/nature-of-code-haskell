module NatureOfCode.Picture where

import Graphics.Gloss.Data.Picture
    ( circleSolid, Picture(Color, Pictures) )
import Graphics.Gloss.Data.Color ( black, greyN)

-- |A solid circle with a border, similar to the examples on the website.
borderedCircle :: Float -> Picture
borderedCircle r = Pictures
    [ Color black $ circleSolid r
    , Color (greyN 0.5) $ circleSolid (r - 2)]
