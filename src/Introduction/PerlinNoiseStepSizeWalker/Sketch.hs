{-# LANGUAGE RecordWildCards #-}
module Introduction.PerlinNoiseWalker.Sketch where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Numeric.Noise.Perlin ( noiseValue, perlin )
import System.Random ( newStdGen, Random(random) )
import NatureOfCode.Picture ( borderedCircle )
import NatureOfCode.Util ( mapRange )

data World = World { currentPosition :: Point
                   , previousPositions :: [Point]
                   , t :: Float }

width :: Int
width = 800

height :: Int
height = 600

minX :: Float
minX = -maxX

maxX :: Float
maxX = fromIntegral width / 2

minY :: Float
minY = -maxY

maxY :: Float
maxY = fromIntegral height / 2

main :: IO ()
main = do
    g <- newStdGen
    let (seed, _) = random g
    simulate
        (InWindow "Perlin noise step size walker" (width, height) (50, 50))
        white
        20
        initial
        view
        (update seed)

view :: World -> Picture
view w = line (previousPositions w)

update :: Int -> ViewPort -> Float -> World -> World
update seed _ dt World{..} =
    let perlinNoise = perlin seed 5 0.05 0.5
        px = realToFrac $ noiseValue perlinNoise (realToFrac t, 0, 0)
        py = realToFrac $ noiseValue perlinNoise (0, realToFrac t, 0)
        newPosition = currentPosition P.+ (px, py)
    in World { currentPosition = newPosition
             , previousPositions = currentPosition : previousPositions
             , t = t + dt }

initial :: World
initial = World { currentPosition = (0, 0)
                , previousPositions = []
                , t = 0 }
