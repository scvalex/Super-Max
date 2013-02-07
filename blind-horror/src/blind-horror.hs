module Main where

import Data.Monoid ( mconcat )
import Graphics.Gloss.Interface.Pure.Game ( Event, play
                                          , Display(..)
                                          , Picture(..)
                                          , black, white )
import Text.Printf ( printf )

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'tickWorld').
data World = W { getTime :: Float
               } deriving ( Eq, Show )

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

----------------------
-- Gloss functions
----------------------

canvasSize :: Int
canvasSize = 1000

windowParams :: Display
windowParams = InWindow "Blind Horror" (canvasSize, canvasSize) (0, 0)

fps :: Int
fps = 10

initWorld :: World
initWorld = W { getTime = 0.0
              }

worldToScene :: World -> Picture
worldToScene w =
    -- The origin (x = 0.0, y = 0.0) is in the bottom left corner.
    Translate (fromIntegral (-canvasSize `div` 2)) (fromIntegral (-canvasSize `div` 2)) $
    -- We draw on a (x = 0.0 -- 1.0, y = 0.0 -- 1.0) sized canvas.
    Scale (fromIntegral canvasSize) (fromIntegral canvasSize) $
    mconcat [ wireframe
            , survivalTime
            ]
  where
    wireframe = Color white (Line [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0), (0.0, 0.0)])

    -- Survival time in top-left corner
    survivalTime = Translate 0.04 0.9 $ Color white (bigText (formatSeconds (getTime w)))

    -- I don't know exactly how big this is, but it's pretty huge.
    hugeText = Scale (1.0 / fromIntegral canvasSize) (1.0 / fromIntegral canvasSize) . Text

    bigText = Scale 0.5 0.5 . hugeText

handleEvent :: Event -> World -> World
handleEvent _ w = w

tickWorld :: Float -> World -> World
tickWorld t w = w { getTime = getTime w + t }

----------------------
-- Helpers
----------------------

formatSeconds :: Float -> String
formatSeconds t = let secs = floor t :: Int
                      mins = secs `div` 60
                      hs   = mins `div` 60
                  in printf "%02d:%02d:%02d.%d"
                         hs (mins `mod` 60)
                         (secs `mod` 60) ((floor ((t - fromIntegral secs) * 10.0) :: Int) `mod` 10)
