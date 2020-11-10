module TemplateSimulate where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )

type World = Int

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main =
    simulate
        (InWindow "<Title goes here>" (width, height) (50, 50))
        white
        20
        initial
        view
        update

view :: World -> Picture
view world = Blank

update :: ViewPort -> Float -> World -> World
update _ _ world = world

initial :: World
initial = 0
