{-# LANGUAGE RecordWildCards #-}

module Vectors.DynamicRandomWalkerWithVectors.Sketch where

import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector (normalizeV)
import Graphics.Gloss.Interface.IO.Interact (Event (..))
import System.Random (Random (random, randomR), StdGen, newStdGen)
import Prelude hiding (Left, Right)

data World = World
  { rndGen :: StdGen,
    mousePosition :: Point,
    currentPosition :: Point,
    previousPositions :: [Point]
  }

data Direction = Up | Down | Left | Right deriving (Enum, Bounded, Eq, Show)

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
view World {previousPositions = ps} = line ps

inputHandler :: Event -> World -> World
inputHandler (EventMotion p) world = world {mousePosition = p}
inputHandler _ world = world

update :: Float -> World -> World
update _ world@World {..} =
  let (newPosition, g') = getNewPosition world
   in world
        { rndGen = g',
          currentPosition = newPosition,
          previousPositions = currentPosition : previousPositions
        }

-- | Move to the mouse or in a random direction with a 50/50 chance.
getNewPosition :: World -> (Point, StdGen)
getNewPosition World {..} =
  if toMouse
    then (moveTo currentPosition mousePosition, g')
    else randomStep g' currentPosition
  where
    (toMouse, g') = random rndGen

initial :: StdGen -> World
initial g =
  World
    { rndGen = g,
      mousePosition = (0, 0),
      currentPosition = (0, 0),
      previousPositions = []
    }

randomStep :: StdGen -> Point -> (Point, StdGen)
randomStep g p =
  let (x, g') = randomR (-1, 1) g
      (y, g'') = randomR (-1, 1) g'
      newPoint = (P.+) p (x, y)
   in (newPoint, g'')

moveTo :: Point -> Point -> Point
moveTo p1 p2 = (P.+) p1 $ normalizeInclZero $ p2 P.- p1

normalizeInclZero :: Point -> Point
normalizeInclZero (0, 0) = (0, 0)
normalizeInclZero v = normalizeV v
