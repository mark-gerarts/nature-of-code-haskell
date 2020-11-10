module Main where

import Data.List ( intercalate )

main :: IO ()
main = putStrLn $ intercalate "\n" [
        "A main application is not implemented.",
        "Run each sketch individually using the following command:",
        "",
        "    stack runghc src/<chapter>/<name>/Sketch.hs",
        "",
        "E.g. to run the first example:",
        "",
        "    stack runghc src/Introduction/TraditionalRandomWalk/Sketch.hs",
        ""
    ]
