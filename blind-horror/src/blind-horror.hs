module Main where

import Graphics.Gloss.Interface.Pure.Game ( Event, play
                                          , Display(..)
                                          , Picture(..)
                                          , black)

type World = ()

main :: IO ()
main = do
    putStrLn "Everything is fine."
    putStrLn "There is nothing to worry about."
    putStrLn "ohgod ohgod ohgod"

    play
        windowParams
        black
        fps
        initWorld
        worldToScene
        handleEvent
        tickWorld

windowParams :: Display
windowParams = FullScreen (1000, 1000)

fps :: Int
fps = 5

initWorld :: World
initWorld = ()

worldToScene :: World -> Picture
worldToScene () = Circle 200

handleEvent :: Event -> World -> World
handleEvent _ () = ()

tickWorld :: Float -> World -> World
tickWorld _ () = ()
