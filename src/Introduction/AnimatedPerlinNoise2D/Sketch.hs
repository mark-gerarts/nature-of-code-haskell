module Introduction.AnimatedPerlinNoise2D.Sketch where

import Graphics.Gloss
import Numeric.Noise.Perlin
import NatureOfCode.Util (mapRange)
import System.Random (random, newStdGen)

type World = Int

width :: Int
width = 800

-- Let's not make the window too big :')
height :: Int
height = 200

main :: IO ()
main = do
    g <- newStdGen
    let (seed, _) = random g
    animate
        (InWindow "2D Perlin noise" (width, height) (50, 50))
        white
        (view seed)

view :: Int -> Float -> Picture
view seed t =
    translate (- fromIntegral width / 2) (- fromIntegral height / 2)
        $ Pictures
        $ map renderPoint grid
    where
        perlinNoise :: Perlin
        perlinNoise = perlin seed 5 0.05 0.5

        pointToAlpha :: Point -> Float
        pointToAlpha (x, y) = mapRange (-1, 1) (0, 1)
            $ realToFrac
            $ noiseValue perlinNoise (realToFrac x, realToFrac y, realToFrac t)

        renderPoint :: Point -> Picture
        renderPoint (x, y) = scale scaleSize scaleSize
            $ Color (withAlpha (pointToAlpha (x, y)) black)
            $ translate x y
            $ rectangleUpperSolid 1 1

        grid :: [Point]
        grid = [ (x, y) | x <- [0..fromIntegral width / scaleSize]
                        , y <- [0..fromIntegral height / scaleSize] ]

        -- Because calculating a whole window of pixels worth of perlin noise is
        -- rather expensive, we increase the performance by lowering the
        -- resolution.
        scaleSize :: Float
        scaleSize = 5
