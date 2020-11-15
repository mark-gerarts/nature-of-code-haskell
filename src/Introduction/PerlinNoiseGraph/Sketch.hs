{-# LANGUAGE RecordWildCards #-}
module Introduction.PerlinNoiseGraph.Sketch where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
    ( Key(Char), Event(EventKey), KeyState(Down) )
import System.Random ( newStdGen, Random(random), StdGen )
import Numeric.Noise.Perlin ( noiseValue, perlin )
import NatureOfCode.Util ( mapRange )
import Numeric ( showFFloat )

data World =
    World { _seed :: Int
          , _octaves :: Int
          , _scale :: Double
          , _persistance :: Double
          , _points :: [Point]
          , _g :: StdGen }

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main = do
    g <- newStdGen
    play
        (InWindow "Perlin noise graph" (width, height) (50, 50))
        white
        5
        (initial g)
        view
        inputHandler
        update

view :: World -> Picture
view w = Pictures
    [ viewGraph w
    , viewCurrentState w
    , viewInstructions ]

viewGraph :: World -> Picture
viewGraph World{..} = moveToBottomRight $ line _points

viewCurrentState :: World -> Picture
viewCurrentState World{..} = Pictures
    $ zipWith viewStat [1..] stats
    where
        stats = [ "Seed: " ++ show _seed
                , "Octaves: " ++ show _octaves
                , "Scale: " ++ showDecimal 3 _scale
                , "Persistance: " ++ showDecimal 1 _persistance ]
        viewStat i = translate 0 (-i*30) . moveToTopLeft . scale 0.2 0.2 . text

viewInstructions :: Picture
viewInstructions = translate 5 5 $ moveToBottomRight $ scale 0.1 0.1 $ text
    "R: new random seed, Q/W: octave +/-, A/S: scale +/-, Z/X: persistance +/-"

inputHandler :: Event -> World -> World
inputHandler (EventKey (Char c) Down _ _) w = calculatePoints $  onKeyDown c w
inputHandler _ w = w

onKeyDown :: Char -> World -> World
onKeyDown 'r' World{..} =
    let (newSeed,g') = random _g
    in calculatePoints $ World{_seed=newSeed, _g=g',..}
onKeyDown 'q' World{..} = World{_octaves = _octaves + 1, ..}
onKeyDown 'w' World{..} = World{_octaves = _octaves - 1, ..}
onKeyDown 'a' World{..} = World{_scale = _scale + 0.001, ..}
onKeyDown 's' World{..} = World{_scale =_scale - 0.001, ..}
onKeyDown 'z' World{..} = World{_persistance = _persistance + 0.1, ..}
onKeyDown 'x' World{..} = World{_persistance = _persistance - 0.1, ..}
onKeyDown _ w = w

update :: Float -> World -> World
update _ world = world

calculatePoints :: World -> World
calculatePoints World{..} =
    let perlinNoise = perlin _seed _octaves _scale _persistance
        xs = [0..width]
        ys = [mapRange (-1, 1) (0, fromIntegral height) $ noiseValue perlinNoise (fromIntegral x, 0, 0) | x <- xs]
        points = zip (map fromIntegral xs) (map realToFrac ys)
    in World{_points=points, ..}

initial :: StdGen -> World
initial g = calculatePoints $
    World { _seed = 1
          , _octaves = 5
          , _scale = 0.001
          , _persistance = 0.5
          , _points = []
          , _g = g }

moveToBottomRight :: Picture -> Picture
moveToBottomRight = translate (- fromIntegral width / 2) (- fromIntegral height / 2)

moveToTopLeft :: Picture -> Picture
moveToTopLeft = translate (- fromIntegral width / 2) (fromIntegral height / 2)

showDecimal :: Int -> Double -> String
showDecimal s x = showFFloat (Just s) (realToFrac x) ""
