module Fractals.RecursiveTree.Sketch where

import Graphics.Gloss

width :: Int
width = 600

height :: Int
height = 400

main :: IO ()
main =
    display
        (InWindow "Recursive tree" (width, height) (50, 50))
        white
        tree

tree :: Picture
tree = translate 0 (- fromIntegral height / 2) $ branch 6 30

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
