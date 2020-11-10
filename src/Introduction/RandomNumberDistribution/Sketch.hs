{-# LANGUAGE RecordWildCards #-}
module Introduction.RandomNumberDistribution where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Random

data World = World { distribution :: Map Int Int
                   , numbers :: [Int]  }

width :: Int
width = 800

height :: Int
height = 200

distributionSize :: Int
distributionSize = 20

main :: IO ()
main = do
    g <- newStdGen
    simulate
        (InWindow "Random number distribution" (width, height) (50, 50))
        white
        20
        (initial g)
        view
        update

view :: World -> Picture
view world =
    -- Start drawing in the bottom left corner of the window.
    translate (- (fromIntegral width / 2)) (-(fromIntegral height / 2))
    $ Pictures
    $ map (uncurry drawRect . (\(k,v) -> (fromIntegral k, fromIntegral v))) (Map.toList (distribution world))
    where
        rectangleWidth = fromIntegral width /  fromIntegral distributionSize

        drawRect i height =
            -- Handle the offset so that each rectangle is next to eachother.
            translate (fromIntegral i * rectangleWidth) 0
            -- Move the bottom left corner on the origin.
            $ translate (rectangleWidth / 2) (height / 2)
            -- Draw a bordered rectangle.
            $ Pictures
                [ rectangleSolid rectangleWidth height
                , Color (greyN 0.4) (rectangleSolid (rectangleWidth - 2) (height - 2))]

update :: ViewPort -> Float -> World -> World
update _ _ World{..} =
    let newNumber = head numbers
        newDistribution = Map.adjust (+1) newNumber distribution
    in World {distribution=newDistribution, numbers=tail numbers}

initial :: StdGen -> World
initial g =
    World { distribution = Map.fromList $ zip [0..distributionSize - 1] (repeat 0)
          , numbers = randomRs (0, distributionSize - 1) g }
