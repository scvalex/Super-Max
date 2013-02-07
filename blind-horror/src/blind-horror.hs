module Main where

import Graphics.Gloss

main :: IO ()
main = do
    putStrLn "Everything is fine."
    putStrLn "There is nothing to worry about."
    putStrLn "ohgod ohgod ohgod"

    play (FullScreen (1000, 1000)) black 5 () (\_ -> Circle 80) (\_ () -> ()) (\_ () -> ())