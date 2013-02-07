module Main where

import Data.Monoid ( Monoid(..) )
import Graphics.Gloss.Interface.Pure.Game ( play
                                          , Event(..), Key(..)
                                          , Display(..)
                                          , Picture(..), Path
                                          , black, greyN, white, orange )
import Text.Printf ( printf )

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'tickWorld').
data World = G { getLevel  :: Int
               , getTime   :: Float
               , getArea   :: Area
               , getPlayer :: Player
               } deriving ( Eq, Show )

-- | The definition of the game area/map.
data Area = Room { getRoomBounds :: (Int, Int, Int, Int) -- ^ the bounds of the room
                                                         -- (these coordinates are not
                                                         -- related to the display ones)
                 , getRoomStart  :: (Int, Int)           -- ^ the player's starting point
                 , getRoomExit   :: (Int, Int)            -- ^ the area's exit point
                 } deriving ( Eq, Show )

data Player = Player { getPlayerPosition :: (Int, Int)
                     , getPlayerMovement :: Maybe Movement
                     } deriving ( Eq, Show )

data Movement = N | S | W | E
              deriving ( Eq, Show )

main :: IO ()
main = do
    putStrLn "Everything is fine."
    putStrLn "There is nothing to worry about."
    putStrLn "ohgod ohgod ohgod"

    play
        windowParams
        black
        fps
        (initWorld area1)
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

area1 :: Area
area1 = Room { getRoomBounds = (0, 0, 100, 100)
             , getRoomStart = (49, 5)
             , getRoomExit = (49, 94)
             }

initWorld :: Area -> World
initWorld area = G { getLevel = 1
                   , getTime  = 0.0
                   , getArea  = area
                   , getPlayer = Player { getPlayerPosition = getRoomStart area
                                        , getPlayerMovement = Nothing
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
            , player
            , hud
            ]
  where
    -- The wireframe in the background.
    wireframe = Color (greyN 0.1) $
                mconcat $
                flip map [0.1, 0.2 .. 0.9] $
                \i -> mconcat [Line [(i, 0.0), (i, 1.0)], Line [(0.0, i), (1.0, i)]]

    -- The player.
    player =
        fromRoomCoordinates $
        let (xp, yp) = getPlayerPosition (getPlayer w)
        in Color orange $
           intPolygon [(xp, yp), (xp, yp + 1), (xp + 1, yp + 1), (xp + 1, yp)]

    -- The current room/map/area.
    room =
        fromRoomCoordinates $
        mconcat [ roomWalls
                , roomExit
                ]

    roomWalls = let (x1, y1, x2, y2) = getRoomBounds (getArea w)
                in intLine [(x1, y1), (x1, y2), (x2, y2), (x2, y1), (x1, y1)]

    roomExit = let (xe, ye) = getRoomExit (getArea w)
               in mconcat [ intPolygon [(xe, ye), (xe + 2, ye), (xe + 2, ye + 1), (xe, ye + 1)]
                          , Translate (fromIntegral (xe - 1)) (fromIntegral (ye + 2)) $ Scale 0.02 0.01 $ Text "Exit"
                          ]

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

    -- Convert a picture in room coordinates to one in drawing coordinates.
    fromRoomCoordinates :: Picture -> Picture
    fromRoomCoordinates =
        let (x1, y1, x2, y2) = getRoomBounds (getArea w) in
        Translate (fromIntegral x1) (fromIntegral y1) .
        Scale (1.0 / fromIntegral (x2 - x1)) (1.0 / fromIntegral (y2 - y1))

    -- Other text sizes (relative to huge text)
    bigText    = Scale 0.5 0.5 . hugeText
    mediumText = Scale 0.25 0.25 . hugeText
    smallText  = Scale 0.1 0.1 . hugeText

handleEvent :: Event -> World -> World
handleEvent ev w =
    let p = getPlayer w
        (x, y) = getPlayerPosition p in
    case ev of
        (EventKey (Char 'a') _ _ _) ->
            w { getPlayer = p { getPlayerPosition = inBounds (x - 1, y) } }
        (EventKey (Char 'd') _ _ _) ->
            w { getPlayer = p { getPlayerPosition = inBounds (x + 1, y) } }
        (EventKey (Char 's') _ _ _) ->
            w { getPlayer = p { getPlayerPosition = inBounds (x, y - 1) } }
        (EventKey (Char 'w') _ _ _) ->
            w { getPlayer = p { getPlayerPosition = inBounds (x, y + 1) } }
        _ ->
            w
  where
    -- Force the coordinates back in the area's bounds.
    inBounds :: (Int, Int) -> (Int, Int)
    inBounds (x, y) =
        case getArea w of
            r@(Room {}) -> let (x1, y1, x2, y2) = getRoomBounds r
                           in (max x1 (min x x2), max y1 (min y y2))

tickWorld :: Float -> World -> World
tickWorld t w = w { getTime = getTime w + t }

-- | How much does the player move for each movement command.
movementDisplacement :: Movement -> (Int, Int)
movementDisplacement N = (0, 1)
movementDisplacement S = (0, -1)
movementDisplacement W = (-1, 0)
movementDisplacement E = (1, 0)

----------------------
-- Helpers
----------------------

formatSeconds :: Float -> String
formatSeconds t = let secs = floor t :: Int
                      mins = secs `div` 60
                  in printf "%02d:%02d.%d"
                         (mins `mod` 60)
                         (secs `mod` 60) ((floor ((t - fromIntegral secs) * 10.0) :: Int) `mod` 10)

-- | Draw a line with 'Int' coordinates.
intLine :: [(Int, Int)] -> Picture
intLine = Line . intPath

-- | Draw a polygon with 'Int' coordinates.
intPolygon :: [(Int, Int)] -> Picture
intPolygon = Polygon . intPath

-- | Convert a path of 'Int' coordinates to a 'Path'.
intPath :: [(Int, Int)] -> Path
intPath = map (\(u, v) -> (fromIntegral u, fromIntegral v))
