module Main where

import Prelude hiding ( foldl )

import Data.Foldable ( foldl )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Game.Engine ( GameEvent(..), play, quitGame
                   , Game, getGameState, setGameState, modifyGameState
                   , Picture(..), Color(..), greyN
                   , Event(..), SDLKey(..), Keysym(..) )
import Scheduler ( Scheduler, newScheduler
                 , runScheduledActions, scheduleAction, dropExpiredActions )
import Spell ( )
import System.Random ( StdGen, newStdGen, randomR )
import Text.Printf ( printf )
import Types ( Direction(..) )
import qualified Data.Map as M
import qualified Data.Set as S

type NpcId = Int

type Level = Int

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'tickWorld').
data World = Game { getState        :: GameState
                  , getScheduler    :: Scheduler World
                  , getLevel        :: Int
                  , getGen          :: StdGen
                  , getTime         :: Double
                  , getTick         :: Int
                  , getArea         :: Area
                  , getPlayer       :: Player
                  , getHeldDownKeys :: Set Keysym -- ^ We get key up and key down events,
                                                  -- but we need to handle holding down a
                                                  -- key ourselves.  We keep track of
                                                  -- which keys are being held down at any
                                                  -- time.  When we update the world, we
                                                  -- process those keys as well as all the
                                                  -- keys that were pressed and released.
                  , getNpcs         :: Map NpcId Npc
                  }

data GameState = PreGame { getObjective :: String }
               | InGame
               | PostGame { getConclusion  :: String
                          , getHasContinue :: Bool }
               deriving ( Eq, Show )

-- | The definition of the game area/map.
data Area = Room { getRoomBounds  :: (Int, Int, Int, Int) -- ^ the bounds of the room
                                                          -- (these coordinates are not
                                                          -- related to the display ones)
                 , getRoomStart   :: (Int, Int)           -- ^ the player's starting point
                 , getRoomExit    :: (Int, Int)           -- ^ the area's exit point
                 } deriving ( Eq, Show )

data Player = Player { getPlayerPosition :: (Int, Int)
                     , getPlayerMovement :: Maybe Direction
                     } deriving ( Eq, Show )

data Npc = Zombie { getNpcId       :: NpcId
                  , getNpcPosition :: (Int, Int)
                  } deriving ( Eq, Show )

main :: IO ()
main = do
    putStrLn "Everything is fine."
    putStrLn "There is nothing to worry about."
    putStrLn "ohgod ohgod ohgod"
    putStrLn ""

    gen <- newStdGen

    play
        (canvasSize, canvasSize)
        tps
        (initWorld gen area1 1)
        worldToScene
        handleEvent

----------------------
-- Gloss functions
----------------------

canvasSize :: Int
canvasSize = 1000

tps :: Int
tps = 10

area1 :: Area
area1 = Room { getRoomBounds = (0, 0, 100, 100)
             , getRoomStart = (49, 5)
             , getRoomExit = (49, 94)
             }

initWorld :: StdGen -> Area -> Level -> World
initWorld gen area lvl =
    let (x1, y1, x2, y2) = getRoomBounds area
        (npcs, gen') =
            foldl (\(ns, gen0) i ->
                    let (xz, gen1) = randomR (x1, x2) gen0
                        (yz, gen2) = randomR (y1, y2) gen1
                        z = Zombie { getNpcId = i, getNpcPosition = (xz, yz) } in
                    (M.insert i z ns, gen2)) (M.empty, gen) [1..2^(lvl - 1)]
        g = Game { getState        = PreGame "Get to the exit"
                 , getScheduler    = newScheduler
                 , getLevel        = lvl
                 , getGen          = gen'
                 , getTime         = 0.0
                 , getTick         = 0
                 , getArea         = area
                 , getHeldDownKeys = S.empty
                 , getNpcs         = npcs
                 , getPlayer       = Player { getPlayerPosition = getRoomStart area
                                            , getPlayerMovement = Nothing
                                            }
                 }
    in scheduleIn 1 movePlayer $
       scheduleIn 2 moveNpcs $
       g

worldToScene :: World -> Picture
worldToScene w =
    -- The origin is in the bottom right corner
    Translate 0.0 (fromIntegral canvasSize) $
    Scale 1.0 (-1.0) $
    -- We draw on a (x = 0.0 -- 1.0, y = 0.0 -- 1.0) sized canvas.
    Scale (fromIntegral canvasSize) (fromIntegral canvasSize) $
    -- Now, draw the scene
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
                mempty
                -- mconcat $
                -- flip map [0.1, 0.2 .. 0.9] $
                -- \i -> mconcat [Line [(i, 0.0), (i, 1.0)], Line [(0.0, i), (1.0, i)]]

    -- A message shown at the beginning and at the end.
    prePostMessage =
        case getState w of
            pg@(PreGame {}) ->
                mconcat [ Translate 0.25 0.45 $ bigText (getObjective pg)
                        , Translate 0.39 0.40 $ mediumText "<press space>"
                        ]
            InGame {} ->
                mempty
            pg@(PostGame { getHasContinue = c }) ->
                mconcat [ Translate 0.36 0.45 $ bigText (getConclusion pg)
                        , if c
                          then Translate 0.39 0.40 $ mediumText "<press space>"
                          else mempty
                        ]

    -- All the non-player entities.
    npcs =
        fromRoomCoordinates $
        mconcat $
        flip map (M.elems (getNpcs w)) $
        \npc -> case npc of
            z@(Zombie {}) ->
                Color (RGBA 255 0 0 255) $
                personPicture (getNpcPosition z)

    -- The player.
    player =
        fromRoomCoordinates $
        let pos = getPlayerPosition (getPlayer w) in
        Color (RGBA 255 140 0 255) $
        personPicture pos

    -- The current room/map/area.
    room =
        fromRoomCoordinates $
        mconcat [ roomWalls
                , roomExit
                ]

    roomWalls = mempty
                -- let (x1, y1, x2, y2) = getRoomBounds (getArea w) in
                -- intLine [(x1, y1), (x1, y2 + 1), (x2 + 1, y2 + 1), (x2 + 1, y1), (x1, y1)]

    roomExit = let (xe, ye) = getRoomExit (getArea w) in
               mconcat [ Color (RGBA 255 215 0 255) $
                         intRectangle xe ye 2 1
                       , Translate (fromIntegral (xe - 1)) (fromIntegral (ye + 2)) $
                         Scale 0.02 0.01 $
                         smallText "Exit"
                       ]

    -- The HUD is overlayed on the game.
    hud = mconcat [ survivalTime
                  , currentLevel
                  ]

    -- Survival time in top-left corner
    survivalTime = Translate 0.04 0.9 $ (bigText (formatSeconds (getTime w)))

    -- Current level in the top-left corner
    currentLevel = Translate 0.04 0.86 $ (mediumText (printf "Level: %d" (getLevel w)))

    -- Draw the picture of a person.
    personPicture (xp, yp) =
        intRectangle xp yp 1 1

    -- Convert a picture in room coordinates to one in drawing coordinates.
    fromRoomCoordinates :: Picture -> Picture
    fromRoomCoordinates =
        let (x1, y1, x2, y2) = getRoomBounds (getArea w) in
        Translate (fromIntegral x1) (fromIntegral y1) .
        Scale (1.0 / fromIntegral (x2 - x1)) (1.0 / fromIntegral (y2 - y1))

    -- Text with fixed sizes
    bigText    = SizedText 40
    mediumText = SizedText 30
    smallText  = SizedText 20

handleEvent :: GameEvent -> Game World ()
handleEvent (InputEvent ev) = handleInputEvent ev
handleEvent (Tick (_, t)) = handleTick t

handleInputEvent :: Event -> Game World ()
handleInputEvent ev = handleGlobalKey ev $ do
    w <- getGameState
    case getState w of
        PreGame {}  -> do
            case ev of
                KeyUp (Keysym { symKey = SDLK_SPACE }) -> do
                    setGameState (w { getState = InGame })
                _ -> do
                    handleInGameEvent
        InGame -> do
            handleInGameEvent
        PostGame { getHasContinue = c } -> do
            case ev of
                KeyUp (Keysym { symKey = SDLK_SPACE }) | c ->
                    setGameState (initWorld (getGen w) (getArea w) (getLevel w + 1))
                _ -> do
                    return ()
  where
    handleInGameEvent = do
        w <- getGameState
        let keys = getHeldDownKeys w
        case ev of
            KeyDown key -> do
                setGameState (w { getHeldDownKeys = S.insert key keys })
                processKey key
            KeyUp key -> do
                setGameState (w { getHeldDownKeys = S.delete key keys })
            _ -> do
                return ()

    -- Handle keys that work regardless of gamestate
    handleGlobalKey :: Event -> Game World () -> Game World ()
    handleGlobalKey (KeyUp (Keysym {symKey = SDLK_ESCAPE})) _ = do
        quitGame
    handleGlobalKey _ fallback = do
        fallback

-- | A key is pressed -- update the world accordingly.
processKey :: Keysym -> Game World ()
processKey key = do
    w <- getGameState
    let p = getPlayer w
    case key of
        Keysym { symKey = SDLK_LEFT } ->
            setGameState (w { getPlayer = p { getPlayerMovement = Just West } })
        Keysym { symKey = SDLK_RIGHT } ->
            setGameState (w { getPlayer = p { getPlayerMovement = Just East } })
        Keysym { symKey = SDLK_DOWN } ->
            setGameState (w { getPlayer = p { getPlayerMovement = Just South } })
        Keysym { symKey = SDLK_UP } ->
            setGameState (w { getPlayer = p { getPlayerMovement = Just North } })
        _ ->
            return ()

handleTick :: Double -> Game World ()
handleTick t = do
    w0 <- getGameState
    case getState w0 of
        PreGame {}  -> return ()
        InGame      -> tickWorldInGame
        PostGame {} -> return ()
  where
    tickWorldInGame :: Game World ()
    tickWorldInGame =
        sequence_ [ processHeldDownKeys
                  , updateTick
                  , updateTime
                  , runPendingActions
                  , checkVictory
                  ]

    -- Increment the tick count.
    updateTick :: Game World ()
    updateTick = do
        modifyGameState (\w -> w { getTick  = getTick w + 1 })

    -- Increment the ticker by the elapsed amount of time.
    updateTime :: Game World ()
    updateTime = do
        modifyGameState (\w -> w { getTime = getTime w + t })

    -- Run actions pending in the scheduler.
    runPendingActions :: Game World ()
    runPendingActions = do
        w <- getGameState
        let w' = runScheduledActions (getTick w) w (getScheduler w)
        setGameState (w' { getScheduler = dropExpiredActions (getTick w') (getScheduler w') })

    -- Some keys were held down, so we didn't see them "happen" this turn.  Simulate key
    -- presses for all keys that are currently being held down.
    processHeldDownKeys :: Game World ()
    processHeldDownKeys = do
        w <- getGameState
        mapM_ processKey (S.toList (getHeldDownKeys w))

    -- Check if the player has lost yet.
    checkVictory :: Game World ()
    checkVictory = do
        w <- getGameState
        let npcPoss = map getNpcPosition $ M.elems (getNpcs w)
            pos = getPlayerPosition (getPlayer w)
        if pos `elem` npcPoss
            then setGameState (w { getState = PostGame { getConclusion  = "You died"
                                                       , getHasContinue = False } })
            else if pos == getRoomExit (getArea w)
            then setGameState (w { getState = PostGame { getConclusion  = "You win"
                                                       , getHasContinue = True } })
            else return ()

----------------------
-- World updates
----------------------

-- Move the player according to its movement, then, reset its movement.
movePlayer :: World -> World
movePlayer w =
    let p = getPlayer w
        (x, y) = getPlayerPosition p
        w' = scheduleIn 1 movePlayer w
    in case getPlayerMovement p of
        Nothing ->
            w'
        Just m ->
            let (xd, yd) = movementDisplacement m in
            w' { getPlayer = p { getPlayerPosition = inBounds (x + xd, y + yd)
                               , getPlayerMovement = Nothing } }
  where
    -- Force the coordinates back in the area's bounds.
    inBounds :: (Int, Int) -> (Int, Int)
    inBounds (x, y) =
        case getArea w of
            r@(Room {}) -> let (x1, y1, x2, y2) = getRoomBounds r in
                           (max x1 (min x x2), max y1 (min y y2))

    -- How much does the player move for each movement command.
    movementDisplacement :: Direction -> (Int, Int)
    movementDisplacement North = (0, 1)
    movementDisplacement South = (0, -1)
    movementDisplacement West  = (-1, 0)
    movementDisplacement East  = (1, 0)

-- Move NPCs according the their own rules.
moveNpcs :: World -> World
moveNpcs w =
    let w' = scheduleIn 2 moveNpcs w in
    w' { getNpcs = M.foldl (moveNpc w') (getNpcs w') (getNpcs w') }

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

----------------------
-- Helpers
----------------------

-- | Schedule an action to run in the given number of ticks.
scheduleIn :: Int -> (World -> World) -> World -> World
scheduleIn d update w =
    w { getScheduler = scheduleAction (getScheduler w) (getTick w + d) update }

formatSeconds :: Double -> String
formatSeconds t = let secs = floor t :: Int
                      mins = secs `div` 60 in
                  printf "%02d:%02d.%d"
                         (mins `mod` 60)
                         (secs `mod` 60) ((floor ((t - fromIntegral secs) * 10.0) :: Int) `mod` 10)

-- | Draw a polygon with 'Int' coordinates.
intRectangle :: Int -> Int -> Int -> Int -> Picture
intRectangle x1 y1 w h = FilledRectangle (fromIntegral x1)
                                         (fromIntegral y1)
                                         (fromIntegral w)
                                         (fromIntegral h)
