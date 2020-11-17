module Introduction.WalkerThatTendsToTheRight.Sketch where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import System.Random

data World = World { rndGen :: StdGen
                   , currentPosition :: Position
                   , previousPositions :: [Position] }

type Position = (Float, Float)

data Direction = Up | Down | Left | Right deriving ( Enum, Bounded, Eq, Show )

-- Basically the same as the traditional random walker, but we override the
-- random implementation to favor going to the right.
instance Random Direction where
    randomR _ g =
        let (r, g') = randomR (0.0 :: Float, 1.0 :: Float) g
            a | r < 0.4 = Right
              | r < 0.6 = Left
              | r < 0.8 = Up
              | otherwise = Down
        in (a, g')
    random = randomR (minBound, maxBound)

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main = do
    g <- newStdGen
    simulate
        (InWindow "Walker that tends to move to the right" (width, height) (50, 50))
        white
        20
        (initial g)
        view
        update

view :: World -> Picture
view World{previousPositions=ps} = line ps

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
