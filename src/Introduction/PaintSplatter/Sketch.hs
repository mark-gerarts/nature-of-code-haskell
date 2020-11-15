{-# LANGUAGE FlexibleContexts #-}
module Introduction.PaintSplatter.Sketch where

import Graphics.Gloss
import System.Random ( newStdGen )
import Data.Random ( normal, sample, MonadRandom )
import Control.Monad.State ( evalState )
import Graphics.Gloss.Data.Vector ( argV, magV )
import Graphics.Gloss.Geometry.Angle ( radToDeg )
import NatureOfCode.Util ( mapRange )

type World = Int

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main = do
    gx <- newStdGen
    gy <- newStdGen
    let randomXs = evalState (normals 0 150) gx
        randomYs = evalState (normals 0 150) gy
        randomPoints = take 30 $ zip randomXs randomYs
    display
        (InWindow "Paint splatter" (width, height) (50, 50))
        white
        (view randomPoints)

view :: [Point] -> Picture
view = Pictures . map viewSingle

viewSingle :: Point -> Picture
viewSingle p = Color (withAlpha 0.96 red)
    $ uncurry translate p
    $ getRotationForPoint p
    $ getScaleForPoint p
    $ circleSolid
    $ getSizeForPoint p
    where
        -- Rotates the splatter so that the major axis goes through the origin,
        -- making it look like it splatted away from the center.
        getRotationForPoint p = rotate $ - (radToDeg $ argV p)

        -- Scales the circle into an ellipse, with greater scaling the further
        -- away from the origin.
        getScaleForPoint p = scale 1 (1 / mapRange (0, maxMag) (1, 3) (magV p))

        -- Make further splatters smaller.
        getSizeForPoint p = maxSize - mapRange (0, maxMag / 2) (0, maxSize - 1) (magV p)

        maxMag = magV (fromIntegral width, fromIntegral height)
        maxSize = 60

normals :: MonadRandom m => Float -> Float -> m [Float]
normals a b = (sequence . repeat) (sample (normal a b))
