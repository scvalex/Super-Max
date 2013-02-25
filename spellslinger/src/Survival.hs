module Survival (
        -- * State
        State, initState,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Game.Engine ( GameEvent(..)
                   , Game, getGameState, setGameState, modifyGameState, randomR
                   , Picture(..)
                   , TextAlignment(..)
                   , Color(..), black, greyN
                   , Event(..), SDLKey(..), Keysym(..) )
import GlobalCommand ( GlobalCommand(..) )
import Scheduler ( Scheduler, newScheduler
                 , runScheduledActions, scheduleAction, dropExpiredActions )
import Spell ( )
import Text.Printf ( printf )
import Types ( Direction(..) )
import qualified Data.Map as M
import qualified Data.Set as S

type NpcId = Int

type Level = Int

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'handleTick').
data State =
    State { getState        :: RoundState
          , getScheduler    :: Scheduler State
          , getLevel        :: Int
          , getTime         :: Double
          , getTick         :: Int
          , getArea         :: Area
          , getPlayer       :: Player
          , getHeldDownKeys :: Set Keysym -- ^ We get key up and key down events, but we
                                          -- need to handle holding down a key ourselves.
                                          -- We keep track of which keys are being held
                                          -- down at any time.  When we update the world,
                                          -- we process those keys as well as all the keys
                                          -- that were pressed and released.
          , getNpcs         :: Map NpcId Npc
          }

data RoundState = PreRound { getObjective :: String }
                | InRound
                | PostRound { getConclusion  :: String
                            , getCanContinue :: Bool }
                deriving ( Eq, Show )

-- | The definition of the game area/map.
data Area = Room { getRoomBounds  :: (Int, Int, Int, Int) -- ^ The bounds of the room
                                                          -- (these coordinates are not
                                                          -- related to the display ones).
                                                          -- The top and right bounds are
                                                          -- exclusive.
                 , getRoomStart   :: (Int, Int)           -- ^ the player's starting point
                 , getRoomExit    :: (Int, Int)           -- ^ the area's exit point
                 } deriving ( Eq, Show )

data Player = Player { getPlayerPosition :: (Int, Int)
                     , getPlayerMovement :: Maybe Direction
                     } deriving ( Eq, Show )

data Npc = Zombie { getNpcId       :: NpcId
                  , getNpcPosition :: (Int, Int)
                  } deriving ( Eq, Show )

----------------------
-- Callbacks
----------------------

area1 :: Area
area1 = Room { getRoomBounds = (0, 0, 100, 100)
             , getRoomStart = (49, 5)
             , getRoomExit = (49, 94)
             }

initState :: Level -> Game a State
initState lvl = do
    let area = area1
        (x1, y1, x2, y2) = getRoomBounds area
    npcs <- foldlM (\ns i -> do
                         xz <- randomR (x1, x2)
                         yz <- randomR (y1, y2)
                         let z = Zombie { getNpcId = i, getNpcPosition = (xz, yz) }
                         return (M.insert i z ns))
                   M.empty
                   [1..2^(lvl - 1)]
    let g = State { getState        = PreRound "Get to the exit"
                  , getScheduler    = newScheduler
                  , getLevel        = lvl
                  , getTime         = 0.0
                  , getTick         = 0
                  , getArea         = area
                  , getHeldDownKeys = S.empty
                  , getNpcs         = npcs
                  , getPlayer       = Player { getPlayerPosition = getRoomStart area
                                             , getPlayerMovement = Nothing
                                             }
                  }
    return (scheduleIn 1 movePlayer $
            scheduleIn 2 moveNpcs $
            g)

drawState :: State -> Picture
drawState w =
    mconcat [ wireframe
            , room
            , player
            , npcs
            , hud
            , prePostMessage
            ]
  where
    -- The wireframe in the background.
    wireframe = mappend (Color (greyN 0.1) $
                         FilledRectangle 0.0 0.0 1.0 1.0) $
                Color black $
                mconcat $ [ FilledRectangle (i + 0.001) (j + 0.001) 0.099 0.099
                          | i <- [0.0, 0.1 .. 0.9]
                          , j <- [0.0, 0.1 .. 0.9]
                          ]

    -- A message shown at the beginning and at the end.
    prePostMessage =
        case getState w of
            pg@(PreRound {}) ->
                mconcat [ Translate 0.5 0.45 $ bigText CenterAligned (getObjective pg)
                        , Translate 0.5 0.40 $ mediumText CenterAligned "<press space to start>"
                        ]
            InRound {} ->
                mempty
            pg@(PostRound { getCanContinue = canContinue }) ->
                mconcat [ Translate 0.5 0.45 $ bigText CenterAligned (getConclusion pg)
                        , Translate 0.5 0.40 $ mediumText CenterAligned $
                          if canContinue
                          then "<press space for next level>"
                          else "<press space for main menu>"
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
        roomExit

    roomExit = let (xe, ye) = getRoomExit (getArea w) in
               mconcat [ Color (RGBA 255 215 0 255) $
                         intRectangle xe ye 2 1
                       , Translate (fromIntegral (xe + 1)) (fromIntegral (ye + 1)) $
                         smallText CenterAligned "Exit"
                       ]

    -- The HUD is overlayed on the game.
    hud = mconcat [ survivalTime
                  , currentLevel
                  ]

    -- Survival time in top-left corner
    survivalTime = Translate 0.04 0.94 $ (bigText LeftAligned (formatSeconds (getTime w)))

    -- Current level in the top-left corner
    currentLevel = Translate 0.04 0.91 $ (mediumText LeftAligned (printf "Level: %d" (getLevel w)))

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
    bigText    = Text 40
    mediumText = Text 30
    smallText  = Text 20

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent (InputEvent ev) =
    handleInputEvent ev
handleEvent (Tick (_, t)) = do
    handleTick t
    return Nothing

handleInputEvent :: Event -> Game State (Maybe GlobalCommand)
handleInputEvent ev = handleGlobalKey ev $ do
    w <- getGameState
    case getState w of
        PreRound {}  -> do
            case ev of
                KeyUp (Keysym { symKey = SDLK_SPACE }) -> do
                    setGameState (w { getState = InRound })
                    return Nothing
                _ -> do
                    handleInRoundEvent
                    return Nothing
        InRound -> do
            handleInRoundEvent
            return Nothing
        PostRound { getCanContinue = canContinue } -> do
            case ev of
                KeyUp (Keysym { symKey = SDLK_SPACE })-> do
                    if canContinue
                        then do
                            setGameState =<< initState (getLevel w + 1)
                            return Nothing
                        else do
                            return (Just ToMainMenu)
                _ -> do
                    return Nothing
  where
    handleInRoundEvent :: Game State ()
    handleInRoundEvent = do
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
    handleGlobalKey :: Event -> Game State (Maybe GlobalCommand) -> Game State (Maybe GlobalCommand)
    handleGlobalKey (KeyUp (Keysym {symKey = SDLK_ESCAPE})) _ = do
        return (Just ToMainMenu)
    handleGlobalKey _ fallback = do
        fallback

-- | A key is pressed -- update the world accordingly.
processKey :: Keysym -> Game State ()
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

handleTick :: Double -> Game State ()
handleTick t = do
    w0 <- getGameState
    case getState w0 of
        PreRound {}  -> return ()
        InRound      -> handleTickInRound
        PostRound {} -> return ()
  where
    handleTickInRound :: Game State ()
    handleTickInRound =
        sequence_ [ processHeldDownKeys
                  , updateTick
                  , updateTime
                  , runPendingActions
                  , checkVictory
                  ]

    -- Increment the tick count.
    updateTick :: Game State ()
    updateTick = do
        modifyGameState (\w -> w { getTick  = getTick w + 1 })

    -- Increment the ticker by the elapsed amount of time.
    updateTime :: Game State ()
    updateTime = do
        modifyGameState (\w -> w { getTime = getTime w + t })

    -- Run actions pending in the scheduler.
    runPendingActions :: Game State ()
    runPendingActions = do
        w <- getGameState
        let w' = runScheduledActions (getTick w) w (getScheduler w)
        setGameState (w' { getScheduler = dropExpiredActions (getTick w') (getScheduler w') })

    -- Some keys were held down, so we didn't see them "happen" this turn.  Simulate key
    -- presses for all keys that are currently being held down.
    processHeldDownKeys :: Game State ()
    processHeldDownKeys = do
        w <- getGameState
        mapM_ processKey (S.toList (getHeldDownKeys w))

    -- Check if the player has lost yet.
    checkVictory :: Game State ()
    checkVictory = do
        w <- getGameState
        let npcPoss = map getNpcPosition $ M.elems (getNpcs w)
            pos = getPlayerPosition (getPlayer w)
        if pos `elem` npcPoss
            then setGameState (w { getState = PostRound { getConclusion  = "You died"
                                                        , getCanContinue = False } })
            else if pos == getRoomExit (getArea w)
            then setGameState (w { getState = PostRound { getConclusion  = "You win"
                                                        , getCanContinue = True } })
            else return ()

----------------------
-- State updates
----------------------

-- Move the player according to its movement, then, reset its movement.
movePlayer :: State -> State
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
                           (max x1 (min x (x2 - 1)), max y1 (min y (y2 - 1)))

    -- How much does the player move for each movement command.
    movementDisplacement :: Direction -> (Int, Int)
    movementDisplacement North = (0, 1)
    movementDisplacement South = (0, -1)
    movementDisplacement West  = (-1, 0)
    movementDisplacement East  = (1, 0)

-- Move NPCs according the their own rules.
moveNpcs :: State -> State
moveNpcs w =
    let w' = scheduleIn 2 moveNpcs w in
    w' { getNpcs = M.foldl (moveNpc w') (getNpcs w') (getNpcs w') }

-- Move a single NPC.  Zombies follow the player.  If a zombie tries to move to an
-- occupied space, it doesn't move.
moveNpc :: State -> Map NpcId Npc -> Npc -> Map NpcId Npc
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
scheduleIn :: Int -> (State -> State) -> State -> State
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
