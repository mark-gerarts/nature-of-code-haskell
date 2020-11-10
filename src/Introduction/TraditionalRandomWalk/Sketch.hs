module Introduction.TraditionalRandomWalk where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact ( Event )
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

main :: IO ()
main = do
    g <- newStdGen
    play
        (InWindow "Traditional Random Walker" (800, 600) (50, 50))
        white
        20
        (initial g)
        view
        inputHandler
        update

view :: World -> Picture
view World{previousPositions=ps} = Pictures $ map renderPosition ps
    where
        renderPosition (x, y) = translate x y $ rectangleSolid 1 1

inputHandler :: Event -> World -> World
inputHandler _ w = w

update :: Float -> World -> World
update _ world@World{rndGen=g, currentPosition=p, previousPositions=ps} =
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
