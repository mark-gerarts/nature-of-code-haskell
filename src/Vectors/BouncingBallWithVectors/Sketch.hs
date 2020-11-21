{-# LANGUAGE RecordWildCards #-}

module Vectors.BouncingBallWithVectors.Sketch where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import NatureOfCode.Picture (borderedCircle)

data World = World
  { position :: Point, -- Point and Vector are the same thing in gloss.
    velocity :: Vector
  }

width :: Int
width = 640

height :: Int
height = 360

centerX :: Float
centerX = fromIntegral width / 2

centerY :: Float
centerY = fromIntegral height / 2

main :: IO ()
main =
  simulate
    (InWindow "Bouncing ball with vectors" (width, height) (50, 50))
    white
    30
    initial
    view
    update

view :: World -> Picture
view World {position = (px, py)} =
  translate (- centerX) (- centerY) $
    translate px py $
      borderedCircle 32

update :: ViewPort -> Float -> World -> World
update _ _ World {position = (px, py), velocity = (vx, vy)} =
  let newPx = px + vx
      newPy = py + vy
      newVx = if newPx >= fromIntegral width || newPx <= 0 then - vx else vx
      newVy = if newPy >= fromIntegral height || newPy <= 0 then - vy else vy
   in World {position = (newPx, newPy), velocity = (newVx, newVy)}

initial :: World
initial = World {position = (centerX, centerY), velocity = (2, 6.6)}
