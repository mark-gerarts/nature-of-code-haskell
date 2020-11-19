{-# LANGUAGE RecordWildCards #-}

module Vectors.BouncingBallWithNoVectors.Sketch where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import NatureOfCode.Picture (borderedCircle)

data World = World
  { px :: Float,
    py :: Float,
    vx :: Float,
    vy :: Float
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
    (InWindow "Bouncing ball with no vectors" (width, height) (50, 50))
    white
    30
    initial
    view
    update

view :: World -> Picture
view World {..} =
  translate (- centerX) (- centerY) $
    translate px py $
      borderedCircle 32

update :: ViewPort -> Float -> World -> World
update _ _ World {..} =
  let newPx = px + vx
      newPy = py + vy
      newVx = if newPx >= fromIntegral width || newPx <= 0 then - vx else vx
      newVy = if newPy >= fromIntegral height || newPy <= 0 then - vy else vy
   in World {px = newPx, py = newPy, vx = newVx, vy = newVy}

initial :: World
initial = World {px = centerX, py = centerY, vx = 2, vy = 6.6}
