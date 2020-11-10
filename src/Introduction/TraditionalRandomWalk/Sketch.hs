module Introduction.TraditionalRandomWalk where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import System.Random

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
        newPosition = walk p direction
    in world{rndGen=g', currentPosition=newPosition, previousPositions=p:ps}

initial :: StdGen -> World
initial g = World { rndGen=g
                  , currentPosition=(0, 0)
                  , previousPositions = [] }

walk :: Position -> Direction -> Position
walk (x, y) Up = (x, y + 1)
walk (x, y) Down = (x, y - 1)
walk (x, y) Left = (x - 1, y)
walk (x, y) Right = (x + 1, y)
