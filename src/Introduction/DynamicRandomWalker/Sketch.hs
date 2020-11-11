{-# LANGUAGE RecordWildCards #-}
module Introduction.DynamicRandomWalker.Sketch where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact ( Event(..) )
import System.Random

data World = World { rndGen :: StdGen
                   , mousePosition :: Point
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
    play
        (InWindow "Dynamic random walker" (width, height) (50, 50))
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
inputHandler (EventMotion p) world = world {mousePosition=p}
inputHandler _ world = world

update :: Float -> World -> World
update _ world@World{..} =
    let (newPosition, g') = getNewPosition world
    in world{ rndGen=g'
            , currentPosition=newPosition
            , previousPositions=currentPosition:previousPositions }

-- | Move to the mouse or in a random direction with a 50/50 chance.
getNewPosition :: World -> (Position, StdGen)
getNewPosition World{..} =
    if toMouse
        then (moveTo currentPosition mousePosition, g')
        -- Make sure we continue with the next seed, and not reuse the current
        -- one.
        else let (direction, g'') = random g'
                in (walk currentPosition direction, g'')
    where
        (toMouse, g') = random rndGen

initial :: StdGen -> World
initial g = World { rndGen=g
                  , mousePosition=(0,0)
                  , currentPosition=(0, 0)
                  , previousPositions = [] }

walk :: Position -> Direction -> Position
walk (x, y) Up = (x, y + 1)
walk (x, y) Down = (x, y - 1)
walk (x, y) Left = (x - 1, y)
walk (x, y) Right = (x + 1, y)

-- | We're not dealing with vectors here, since we technically haven't seen
-- that yet. So we use a naive implementation to see which direction is the best
-- move to reach the given point.
moveTo :: Position -> Point -> Position
moveTo (x, y) (x', y') = walk (x, y) direction
    where
        diffX = x' - x
        diffY = y' - y
        direction
            | abs diffX > abs diffY && diffX > 0 = Right
            | abs diffX > abs diffY && diffX < 0 = Left
            | abs diffX < abs diffY && diffY > 0 = Up
            | otherwise = Down
