{-# LANGUAGE RecordWildCards #-}
module Introduction.GuassianDistribution.Sketch where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import System.Random
import Data.Random

data World = World { circles :: [Float]
                   , stdGen :: StdGen }

width :: Int
width = 640

height :: Int
height = 200

main :: IO ()
main = do
    g <- newStdGen
    simulate
        (InWindow "Gaussian distribution" (width, height) (50, 50))
        white
        20
        (initial g)
        view
        update

view :: World -> Picture
view World {..} = Pictures $ map drawCircle circles

update :: ViewPort -> Float -> World -> World
update _ _ World { .. } = World { stdGen=g', circles=x:circles }
    where
        -- Since the gloss origin is in the center, whe can just use 0 as the
        -- mean.
        (x, g') = sampleState (normal 0 60) stdGen

initial :: StdGen -> World
initial g = World { circles=[], stdGen=g }

drawCircle :: Float -> Picture
drawCircle x = translate x 0 $ Color (withAlpha 0.02 black) (circleSolid 16)
