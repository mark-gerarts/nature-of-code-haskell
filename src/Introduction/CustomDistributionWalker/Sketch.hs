module Introduction.CustomDistributionWalker.Sketch where

import Prelude hiding ( Left, Right )
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ( ViewPort )
import System.Random
import NatureOfCode.Util ( mapRange )

data World = World { rndGen :: StdGen
                   , currentPosition :: Position
                   , previousPositions :: [Position] }

type Position = (Float, Float)

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
    let (newPosition, g') = step p g
    in world{rndGen=g', currentPosition=newPosition, previousPositions=p:ps}

initial :: StdGen -> World
initial g = World { rndGen=g
                  , currentPosition=(0, 0)
                  , previousPositions = [] }

step :: Position -> StdGen -> (Position, StdGen)
step (x, y) g =
    let (stepSize, g') = customDistribution g (0, 10)
        (stepX, g'') = randomR (-stepSize, stepSize) g'
        (stepY, g''') = randomR (-stepSize, stepSize) g''
        newPosition = (x + stepX, y + stepY)
    in (newPosition, g''')

customDistribution :: StdGen -> (Float, Float) -> (Float, StdGen)
customDistribution g range =
    let (probability, g') = random g
        (value, g'') = random g'
    in if value >= (probability^2)
        then (mapRange (0, 1) range value, g'')
        else customDistribution g'' range
