module Introduction.GaussianRandomWalk.Sketch where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import System.Random
import Data.Random

data World = World { rndGen :: StdGen
                   , currentPosition :: Position
                   , previousPositions :: [Position] }

type Position = (Float, Float)

data Direction = Up | Down | Left | Right deriving ( Enum, Bounded, Eq, Show )

instance Random Direction where
    randomR (a, b) g =
        case randomR (fromEnum a, fromEnum b) g of
            (x, g') -> (toEnum x, g')
    random = randomR (minBound, maxBound)

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main = do
    g <- newStdGen
    simulate
        (InWindow "Traditional Random Walker" (width, height) (50, 50))
        white
        20
        (initial g)
        view
        update

view :: World -> Picture
view World{previousPositions=ps} = Pictures $ map renderPosition ps
    where
        renderPosition (x, y) = translate x y $ rectangleSolid 1 1

update :: ViewPort -> Float -> World -> World
update _ _ world@World{rndGen=g, currentPosition=p, previousPositions=ps} =
    let (direction, g') = random g
        (distance, g'') = sampleState stdNormal g'
        newPosition = walk p direction (1 + distance)
    in world{rndGen=g'', currentPosition=newPosition, previousPositions=p:ps}

initial :: StdGen -> World
initial g = World { rndGen=g
                  , currentPosition=(0, 0)
                  , previousPositions = [] }

walk :: Position -> Direction -> Float -> Position
walk (x, y) Up d = (x, y + d)
walk (x, y) Down d = (x, y - d)
walk (x, y) Left d = (x - d, y)
walk (x, y) Right d = (x + d, y)
