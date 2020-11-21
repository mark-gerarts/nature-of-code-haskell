module Vectors.VectorSubtraction.Sketch where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Interface.IO.Interact (Event (EventMotion))

type World = (Float, Float)

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main =
  play
    (InWindow "Vector subtraction" (width, height) (50, 50))
    white
    20
    initial
    view
    inputHandler
    update

view :: World -> Picture
view p = line [(0, 0), p P.- (0, 0)]

inputHandler :: Event -> World -> World
inputHandler (EventMotion p) _ = p
inputHandler _ w = w

update :: Float -> World -> World
update _ world = world

initial :: World
initial = (0, 0)
