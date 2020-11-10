module TemplatePlay where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact ( Event )

type World = Int

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main =
    play
        (InWindow "<Title goes here>" (width, height) (50, 50))
        white
        20
        initial
        view
        inputHandler
        update

view :: World -> Picture
view world = Blank

inputHandler :: Event -> World -> World
inputHandler _ w = w

update :: Float -> World -> World
update _ world = world

initial :: World
initial = 0
