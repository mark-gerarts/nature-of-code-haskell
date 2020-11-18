module Introduction.PerlinNoise3D.Sketch where

import Linear (V3 (..))
import NatureOfCode.Util (mapRange)
import Numeric.Noise.Perlin (Perlin, noiseValue, perlin)
import System.Random (Random (random), newStdGen)
import Vis

landscapeWidth :: Double
landscapeWidth = 200

landscapeHeight :: Double
landscapeHeight = 200

landscapeStep :: Double
landscapeStep = 10

main :: IO ()
main =
  do
    g <- newStdGen
    let (seed, _) = random g
    display
      ( defaultOpts
          { optWindowName = "Perlin landscape",
            optBackgroundColor = Just white,
            optWindowSize = Just (800, 600)
          }
      )
      (view seed)

view :: Int -> VisObject Double
view seed =
  VisObjects $ map (pointToQuad seed) grid

grid :: [(Double, Double)]
grid =
  [ (x, y) | x <- [0, landscapeStep .. landscapeWidth], y <- [0, landscapeStep .. landscapeHeight]
  ]

-- | Creates a quad with the bottom left corner in the (x, y) coördinate, and
-- random perlin noise as z values.
pointToQuad :: Int -> (Double, Double) -> VisObject Double
pointToQuad seed (x, y) = borderedQuad p1 p2 p3 p4
  where
    [p1, p2, p3, p4] = [V3 x y (perlinValue x y) | (x, y) <- cornerCoords]

    -- The 4 corner points of the quad of which the bottom left corner is the
    -- point passed to the main function.
    cornerCoords :: [(Double, Double)]
    cornerCoords =
      [ (x, y),
        (x, y + landscapeStep),
        (x + landscapeStep, y + landscapeStep),
        (x + landscapeStep, y)
      ]

    perlinValue :: Double -> Double -> Double
    perlinValue x y = mapRange (-1, 1) (0, 10) $ noiseValue perlinNoise (x, y, 0)

    perlinNoise :: Perlin
    perlinNoise = perlin seed 5 0.05 0.5

-- | Creates a plane (quad) with a black border.
borderedQuad :: Num a => V3 a -> V3 a -> V3 a -> V3 a -> VisObject a
borderedQuad p1 p2 p3 p4 =
  VisObjects
    [ Quad p1 p2 p3 p4 (greyN 0.7),
      Line (Just 2) [p1, p2] black,
      Line (Just 2) [p2, p3] black,
      Line (Just 2) [p3, p4] black,
      Line (Just 2) [p4, p1] black
    ]
