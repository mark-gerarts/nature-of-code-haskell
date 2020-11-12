module Fractals.DynamicRecursiveTree.Sketch where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact ( Event(..) )

-- Our "world" is just the position of the mouse.
type MousePosition = Point

width :: Int
width = 600

height :: Int
height = 400

main :: IO ()
main =
    play
        (InWindow "Recursive tree" (width, height) (50, 50))
        white
        20
        initial
        view
        inputHandler
        update

view :: MousePosition -> Picture
view (x, _) = translate 0 (- fromIntegral height / 2) $ branch 6 theta
    where
        halfWidth = fromIntegral width / 2
        minX = -halfWidth
        maxX = halfWidth
        theta = mapRange (minX, maxX) (0, 90) x

inputHandler :: Event -> MousePosition -> MousePosition
inputHandler (EventMotion p) _ = p
inputHandler _ w = w

update :: Float -> MousePosition -> MousePosition
update _ = id

initial :: MousePosition
initial = (0, 0)

branch :: Int -> Float -> Picture
branch n theta = go n
    where
        go 0 = Blank
        go n' = Pictures
            [ rectangleUpperSolid 2 l
            , Translate 0 l
                $ Rotate theta
                $ go (n' - 1)
            , Translate 0 l
                $ Rotate (-theta)
                $ go (n' - 1) ]
            where
                baseLength = 120
                l = baseLength * 0.66^(n-n')

-- |Credit: https://rosettacode.org/wiki/Map_range#Haskell.
-- This should probably go to a util module.
mapRange :: Fractional a => (a, a) -> (a, a) -> a -> a
mapRange (a1, a2) (b1, b2) s = b1 + (s - a1) * (b2 - b1) / (a2 - a1)
