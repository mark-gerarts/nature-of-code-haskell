module TemplateSimulate where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import Numeric.Noise.Perlin
import System.Random
import NatureOfCode.Picture ( borderedCircle )
import NatureOfCode.Util ( mapRange )

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
    animate
        (InWindow "Perlin noise walker" (width, height) (50, 50))
        white
        (view seed)

view :: Int -> Float -> Picture
view seed t =
    let perlinNoise = perlin seed 5 0.05 0.5
        -- The example in the book uses a tx and ty with different offsets, but
        -- since the noise library we works with vectors instead of a single
        -- value, we can just pass the t as the x or y value resp.
        px = mapRange (-1, 1) (minX, maxX) $ realToFrac $ noiseValue perlinNoise (realToFrac t, 0, 0)
        py = mapRange (-1, 1) (minY, maxY) $ realToFrac $ noiseValue perlinNoise (0, realToFrac t, 0)
    in translate (realToFrac px) (realToFrac py) $ borderedCircle 32
