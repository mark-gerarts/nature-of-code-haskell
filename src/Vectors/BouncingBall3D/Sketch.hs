{-# LANGUAGE RecordWildCards #-}

module Vectors.BouncingBall3D.Sketch where

import Linear (V3 (..), (^+^))
import Vis

data World = World
  { position :: V3 Double,
    velocity :: V3 Double
  }
  deriving (Show)

cubeWidth :: Double
cubeWidth = 100

sphereRadius :: Double
sphereRadius = 10

main :: IO ()
main =
  simulate
    ( defaultOpts
        { optWindowName = "3D bouncing ball",
          optBackgroundColor = Just white,
          optWindowSize = Just (800, 600),
          optInitialCamera = Just initialCamera0
        }
    )
    (1 / 20)
    initial
    view
    update

view :: World -> VisObject Double
view World {..} =
  VisObjects
    [ Cube cubeWidth Wireframe black,
      Trans position $ Sphere sphereRadius Solid (greyN 0.5)
    ]

update :: Float -> World -> World
update _ World {..} =
  let (V3 px py pz) = position ^+^ velocity
      (V3 vx vy vz) = velocity
      vx' = checkBounds px vx
      vy' = checkBounds py vy
      vz' = checkBounds pz vz
   in World {position = V3 px py pz, velocity = V3 vx' vy' vz'}
  where
    checkBounds :: Double -> Double -> Double
    checkBounds px vx =
      if px - sphereRadius <= - halfWidth || px + sphereRadius >= halfWidth
        then - vx
        else vx

    halfWidth :: Double
    halfWidth = cubeWidth / 2

initialCamera0 :: Camera0
initialCamera0 =
  Camera0
    { phi0 = 30,
      theta0 = 20,
      rho0 = 260
    }

initial :: World
initial =
  World
    { position = V3 0 0 0,
      velocity = V3 1 0.33 0.66
    }
