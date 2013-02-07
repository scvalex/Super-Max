module Main where

import Prelude hiding ( foldl )

import Data.Foldable ( foldl )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Map ( Map )
import qualified Data.Map as M
import Graphics.Gloss.Interface.Pure.Game ( play
                                          , Event(..), Key(..), SpecialKey(..), KeyState(..)
                                          , Display(..)
                                          , Picture(..), Path
                                          , dim, black, greyN, white, yellow, orange, red )
import Text.Printf ( printf )

type NpcId = Int

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'tickWorld').
data World = Game { getState        :: GameState
                  , getLevel        :: Int
                  , getTime         :: Float
                  , getArea         :: Area
                  , getPlayer       :: Player
                  , getHeldDownKeys :: Set Key -- ^ We get key up and key down events, but
                                               -- we need to handle holding down a key
                                               -- ourselves.  We keep track of which keys
                                               -- are being held down at any time.  When
                                               -- we update the world, we process those
                                               -- keys as well as all the keys that were
                                               -- pressed and released.
                  , getNpcs         :: Map NpcId Npc
                  }
           deriving ( Eq, Show )

data GameState = PreGame { getObjective :: String }
               | InGame
               | PostGame { getConclusion :: String }
               deriving ( Eq, Show )

-- | The definition of the game area/map.
data Area = Room { getRoomBounds  :: (Int, Int, Int, Int) -- ^ the bounds of the room
                                                          -- (these coordinates are not
                                                          -- related to the display ones)
                 , getRoomStart   :: (Int, Int)           -- ^ the player's starting point
                 , getRoomExit    :: (Int, Int)           -- ^ the area's exit point
                 , getInitialNpcs :: [Npc]                -- ^ the area's initial NPCs
                 } deriving ( Eq, Show )

data Player = Player { getPlayerPosition :: (Int, Int)
                     , getPlayerMovement :: Maybe Movement
                     } deriving ( Eq, Show )

data Movement = N | S | W | E
              deriving ( Eq, Show )

data Npc = Zombie { getNpcId       :: NpcId
                  , getNpcPosition :: (Int, Int)
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
             , getInitialNpcs = [ Zombie 0 (25, 25), Zombie 1 (25, 75)
                                , Zombie 2 (75, 75), Zombie 3 (75, 25) ]
             }

initWorld :: Area -> World
initWorld area =
    Game { getState = PreGame "Get to the exit"
         , getLevel = 1
         , getTime  = 0.0
         , getArea  = area
         , getPlayer = Player { getPlayerPosition = getRoomStart area
                              , getPlayerMovement = Nothing
                              }
         , getHeldDownKeys = S.empty
         , getNpcs = M.fromList $ map (\npc -> (getNpcId npc, npc)) $ getInitialNpcs area
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
            , npcs
            , hud
            , prePostMessage
            ]
  where
    -- The wireframe in the background.
    wireframe = Color (greyN 0.1) $
                mconcat $
                flip map [0.1, 0.2 .. 0.9] $
                \i -> mconcat [Line [(i, 0.0), (i, 1.0)], Line [(0.0, i), (1.0, i)]]

    -- A message shown at the beginning and at the end.
    prePostMessage =
        case getState w of
            pg@(PreGame {}) ->
                Translate 0.25 0.45 $ bigText (getObjective pg)
            InGame {} ->
                mempty
            pg@(PostGame {}) ->
                Translate 0.35 0.45 $ bigText (getConclusion pg)

    -- All the non-player entities.
    npcs =
        fromRoomCoordinates $
        mconcat $
        flip map (M.elems (getNpcs w)) $
        \npc -> case npc of
            z@(Zombie {}) ->
                Color red $
                personPicture (getNpcPosition z)

    -- The player.
    player =
        fromRoomCoordinates $
        let pos = getPlayerPosition (getPlayer w) in
        Color orange $
        personPicture pos

    -- The current room/map/area.
    room =
        fromRoomCoordinates $
        mconcat [ roomWalls
                , roomExit
                ]

    roomWalls = let (x1, y1, x2, y2) = getRoomBounds (getArea w) in
                intLine [(x1, y1), (x1, y2 + 1), (x2 + 1, y2 + 1), (x2 + 1, y1), (x1, y1)]

    roomExit = let (xe, ye) = getRoomExit (getArea w) in
               mconcat [ Color (dim yellow) $
                         intPolygon [(xe, ye), (xe + 2, ye), (xe + 2, ye + 1), (xe, ye + 1)]
                       , Translate (fromIntegral (xe - 1)) (fromIntegral (ye + 2)) $
                         Scale 0.02 0.01 $
                         Text "Exit"
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

    -- Draw the picture of a person.
    personPicture (xp, yp) =
        intPolygon [(xp, yp), (xp, yp + 1), (xp + 1, yp + 1), (xp + 1, yp)]

    -- Convert a picture in room coordinates to one in drawing coordinates.
    fromRoomCoordinates :: Picture -> Picture
    fromRoomCoordinates =
        let (x1, y1, x2, y2) = getRoomBounds (getArea w) in
        Translate (fromIntegral x1) (fromIntegral y1) .
        Scale (1.0 / fromIntegral (x2 - x1)) (1.0 / fromIntegral (y2 - y1))

    -- Other text sizes (relative to huge text)
    bigText    = Scale 0.5 0.5 . hugeText
    mediumText = Scale 0.25 0.25 . hugeText

handleEvent :: Event -> World -> World
handleEvent ev w =
    case getState w of
        PreGame {}  ->
            case ev of
                -- KeyUnknown seems to be sent to the program on startup.
                EventKey (SpecialKey KeyUnknown) _ _ _ -> w
                EventKey _ Up _ _                      -> w { getState = InGame }
                _                                      -> w
        InGame      -> handleInGameEvent
        PostGame {} -> w
  where
    handleInGameEvent =
        let keys = getHeldDownKeys w in
        case ev of
            EventKey key Down _ _ ->
                processKey (w { getHeldDownKeys = S.insert key keys }) key
            EventKey key Up _ _ ->
                w { getHeldDownKeys = S.delete key keys }
            _ ->
                w

-- | A key is pressed -- update the world accordingly.
processKey :: World -> Key -> World
processKey w key =
    let p = getPlayer w in
    case key of
        Char 'a' ->
            w { getPlayer = p { getPlayerMovement = Just W } }
        Char 'd' ->
            w { getPlayer = p { getPlayerMovement = Just E } }
        Char 's' ->
            w { getPlayer = p { getPlayerMovement = Just S } }
        Char 'w' ->
            w { getPlayer = p { getPlayerMovement = Just N } }
        _ ->
            w

tickWorld :: Float -> World -> World
tickWorld t w0 =
    case getState w0 of
        PreGame {}  -> w0
        InGame      -> tickWorldInGame
        PostGame {} -> w0
  where
    tickWorldInGame =
        let w1 = foldl processKey w0 (getHeldDownKeys w0)
            w2 = w1 { getPlayer = movePlayer w1 }
            w3 = w2 { getNpcs = moveNpcs w2 }
            w4 = updateTime w3
            w5 = checkVictory w4
        in w5

    -- Check if the player has lost yet.
    checkVictory :: World -> World
    checkVictory w =
        let npcPoss = map getNpcPosition $ M.elems (getNpcs w)
            pos = getPlayerPosition (getPlayer w) in
        if pos `elem` npcPoss
        then w { getState = PostGame { getConclusion = "You died" } }
        else if pos == getRoomExit (getArea w)
             then w { getState = PostGame { getConclusion = "You win" } }
             else w

    -- Increment the ticker by the elapsed amount of time.
    updateTime :: World -> World
    updateTime w = w { getTime = getTime w + t }

    -- Move NPCs according the their own rules.
    moveNpcs :: World -> Map NpcId Npc
    moveNpcs w = M.foldl (moveNpc w) (getNpcs w) (getNpcs w)

    -- Move a single NPC.  Zombies follow the player.  If a zombie tries to move to an
    -- occupied space, it doesn't move.
    moveNpc :: World -> Map NpcId Npc -> Npc -> Map NpcId Npc
    moveNpc w npcs z@(Zombie {}) =
        let (xz, yz) = getNpcPosition z
            (xp, yp) = getPlayerPosition (getPlayer w)
            pos' = if abs (xp - xz) > abs (yp - yz)
                   then (xz + signum (xp - xz), yz)
                   else (xz, yz + signum (yp - yz)) in
        if not (posOccupied pos' npcs)
        then M.insert (getNpcId z) (z { getNpcPosition = pos' }) npcs
        else npcs
      where
        posOccupied pos = M.foldl (\o npc -> o || getNpcPosition npc == pos) False

    -- Move the player according to its movement, then, reset its movement.
    movePlayer :: World -> Player
    movePlayer w =
        let p = getPlayer w
            (x, y) = getPlayerPosition p in
        case getPlayerMovement p of
            Nothing ->
                p
            Just m ->
                    let (xd, yd) = movementDisplacement m in
                    p { getPlayerPosition = inBounds w (x + xd, y + yd)
                      , getPlayerMovement = Nothing }

    -- Force the coordinates back in the area's bounds.
    inBounds :: World -> (Int, Int) -> (Int, Int)
    inBounds w (x, y) =
        case getArea w of
            r@(Room {}) -> let (x1, y1, x2, y2) = getRoomBounds r in
                           (max x1 (min x x2), max y1 (min y y2))

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
                      mins = secs `div` 60 in
                  printf "%02d:%02d.%d"
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
