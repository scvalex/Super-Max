module Main where

import Data.Monoid ( Monoid(..) )
import Graphics.Gloss.Interface.Pure.Game ( Event, play
                                          , Display(..)
                                          , Picture(..)
                                          , black, white )
import Text.Printf ( printf )

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'tickWorld').
data World = W { getLevel :: Int
               , getTime  :: Float
               , getArea  :: Area
               } deriving ( Eq, Show )

-- | The definition of the game area/map.
data Area = Room { getRoomBounds :: (Int, Int, Int, Int) -- ^ the bounds of the room
                                                         -- (these coordinates are not
                                                         -- related to the display ones)
                 , getRoomStart :: (Int, Int)            -- ^ the player's starting point
                 , getRoomExit :: (Int, Int)             -- ^ the area's exit point
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
initWorld = W { getLevel = 1
              , getTime  = 0.0
              , getArea  = Room { getRoomBounds = (0, 0, 100, 100)
                                , getRoomStart = (49, 5)
                                , getRoomExit = (49, 94)
                                }
              }

worldToScene :: World -> Picture
worldToScene w =
    -- The origin (x = 0.0, y = 0.0) is in the bottom left corner.
    Translate (fromIntegral (-canvasSize `div` 2)) (fromIntegral (-canvasSize `div` 2)) $
    -- We draw on a (x = 0.0 -- 1.0, y = 0.0 -- 1.0) sized canvas.
    Scale (fromIntegral canvasSize) (fromIntegral canvasSize) $
    -- Just default all the foreground colours to white.
    Color white $
    mconcat [ wireframe
            , room
            , hud
            ]
  where
    -- The wireframe in the background.
    wireframe = Line [(0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0), (0.0, 0.0)]

    -- The current room/map/area.
    room = mempty

    -- The HUD is overlayed on the game.
    hud = mconcat [ survivalTime
                  , currentLevel
                  ]

    -- Survival time in top-left corner
    survivalTime = Translate 0.04 0.9 $ (bigText (formatSeconds (getTime w)))

    -- Current level in the top-left corner
    currentLevel = Translate 0.04 0.86 $ (mediumText (printf "Level: %d" (getLevel w)))

    -- I don't know exactly how big this is, but it's pretty huge.
    hugeText = Scale (1.0 / fromIntegral canvasSize) (1.0 / fromIntegral canvasSize) . Text

    -- Other text sizes (relative to huge text)
    bigText    = Scale 0.5 0.5 . hugeText
    mediumText = Scale 0.25 0.25 . hugeText
    smallText  = Scale 0.1 0.1 . hugeText

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
                  in printf "%02d:%02d.%d"
                         (mins `mod` 60)
                         (secs `mod` 60) ((floor ((t - fromIntegral secs) * 10.0) :: Int) `mod` 10)
