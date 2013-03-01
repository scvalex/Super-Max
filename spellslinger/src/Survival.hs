module Survival (
        -- * State
        State, initState,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Common ( intRectangle, fromAreaCoordinates )
import Control.Applicative ( (<$>) )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Game.Engine ( GameEvent(..)
                   , Game, getsGameState, modifyGameState
                   , Picture(..)
                   , TextAlignment(..)
                   , Color(..), black, greyN
                   , Event(..), SDLKey(..), Keysym(..) )
import Game.Entity ( SomeEntity(..) )
import GlobalCommand ( GlobalCommand(..) )
import Scheduler ( Scheduler, newScheduler
                 , runScheduledActions, scheduleAction, dropExpiredActions )
import Spell ( )
import Text.Printf ( printf )
import Types ( Direction(..), EntityId(..), Position(..) )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Game.Entity as Entity
import qualified Zombie as Zombie

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
          , getEntities     :: Map EntityId SomeEntity
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
                 , getRoomStart   :: Position             -- ^ the player's starting point
                 , getRoomExit    :: Position             -- ^ the area's exit point
                 } deriving ( Eq, Show )

data Player = Player { getPlayerPosition :: Position
                     , getPlayerMovement :: Maybe Direction
                     } deriving ( Eq, Show )

----------------------
-- Callbacks
----------------------

area1 :: Area
area1 = Room { getRoomBounds = (0, 0, 100, 100)
             , getRoomStart = Position (49, 5)
             , getRoomExit = Position (49, 94)
             }

initState :: Level -> Game a State
initState lvl = do
    let area = area1
        roomBounds = getRoomBounds area
    entities <- foldlM (\ns _ -> do
                             z <- Entity.init (Zombie.RandomZombie
                                                   { Zombie.getAreaBounds = roomBounds
                                                   })
                             return (M.insert (Entity.entityId z) (SomeEntity z) ns))
                       M.empty
                       [(1 :: Int) ..2^(lvl - 1)]
    return (State { getState        = PreRound "Get to the exit"
                  , getScheduler    = newScheduler
                  , getLevel        = lvl
                  , getTime         = 0.0
                  , getTick         = 0
                  , getArea         = area
                  , getHeldDownKeys = S.empty
                  , getEntities     = entities
                  , getPlayer       = Player { getPlayerPosition = getRoomStart area
                                             , getPlayerMovement = Nothing
                                             }
                  })

drawState :: State -> Picture
drawState w =
    mconcat [ wireframe
            , room
            , player
            , entities
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
    entities =
        mconcat $
        map (\(SomeEntity e) -> Entity.draw e) (M.elems (getEntities w))

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

    roomExit = let Position (xe, ye) = getRoomExit (getArea w) in
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
    personPicture (Position (xp, yp)) =
        intRectangle xp yp 1 1

    -- Convert a picture in room coordinates to one in drawing coordinates.
    fromRoomCoordinates :: Picture -> Picture
    fromRoomCoordinates = fromAreaCoordinates (getRoomBounds (getArea w))

    -- Text with fixed sizes
    bigText    = Text 40
    mediumText = Text 30
    smallText  = Text 20

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent (InputEvent ev) =
    handleInputEvent ev
handleEvent (Tick 0 (_, t)) = do
    -- FIXME Using the first tick to prime the scheduler seems weird.
    -- First tick.  Prime the world.
    scheduleIn 0 movePlayer
    scheduleIn 1 tickEntities
    handleTick t
    return Nothing
handleEvent (Tick _ (_, t)) = do
    handleTick t
    return Nothing

handleInputEvent :: Event -> Game State (Maybe GlobalCommand)
handleInputEvent ev = handleGlobalKey ev $ do
    state <- getsGameState getState
    case state of
        PreRound {}  -> do
            case ev of
                KeyUp (Keysym { symKey = SDLK_SPACE }) -> do
                    modifyGameState (\w' -> w' { getState = InRound })
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
                            lvl <- getsGameState getLevel
                            w' <- initState (lvl + 1)
                            modifyGameState (\_ -> w')
                            return Nothing
                        else do
                            return (Just ToMainMenu)
                _ -> do
                    return Nothing
  where
    handleInRoundEvent :: Game State ()
    handleInRoundEvent = do
        keys <- getsGameState getHeldDownKeys
        case ev of
            KeyDown key -> do
                modifyGameState (\w -> w { getHeldDownKeys = S.insert key keys })
                processKey key
            KeyUp key -> do
                modifyGameState (\w -> w { getHeldDownKeys = S.delete key keys })
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
    p <- getsGameState getPlayer
    case key of
        Keysym { symKey = SDLK_LEFT } ->
            modifyGameState (\w -> w { getPlayer = p { getPlayerMovement = Just West } })
        Keysym { symKey = SDLK_RIGHT } ->
            modifyGameState (\w -> w { getPlayer = p { getPlayerMovement = Just East } })
        Keysym { symKey = SDLK_DOWN } ->
            modifyGameState (\w -> w { getPlayer = p { getPlayerMovement = Just South } })
        Keysym { symKey = SDLK_UP } ->
            modifyGameState (\w -> w { getPlayer = p { getPlayerMovement = Just North } })
        _ ->
            return ()

handleTick :: Double -> Game State ()
handleTick t = do
    state <- getsGameState getState
    case state of
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
        -- FIXME Move the tick counter to the engine state.
        tick <- getsGameState getTick
        scheduler <- getsGameState getScheduler
        scheduler' <- runScheduledActions tick scheduler
        modifyGameState (\w -> w { getScheduler = dropExpiredActions tick scheduler' })

    -- Some keys were held down, so we didn't see them "happen" this turn.  Simulate key
    -- presses for all keys that are currently being held down.
    processHeldDownKeys :: Game State ()
    processHeldDownKeys = do
        keys <- getsGameState getHeldDownKeys
        mapM_ processKey (S.toList keys)

    -- Check if the player has lost yet.
    checkVictory :: Game State ()
    checkVictory = do
        entityPoss <- map (\(SomeEntity e) -> Entity.entityPosition e) .
                      M.elems <$>
                      getsGameState getEntities
        area <- getsGameState getArea
        pos <- getPlayerPosition <$> getsGameState getPlayer
        if pos `elem` entityPoss
            then modifyGameState (\w -> w { getState = PostRound { getConclusion  = "You died"
                                                                 , getCanContinue = False } })
            else if pos == getRoomExit area
            then modifyGameState (\w -> w { getState = PostRound { getConclusion  = "You win"
                                                                 , getCanContinue = True } })
            else return ()

----------------------
-- State updates
----------------------

-- Move the player according to its movement, then, reset its movement.
movePlayer :: Game State ()
movePlayer = do
    p <- getsGameState getPlayer
    let Position (x, y) = getPlayerPosition p
    scheduleIn 1 movePlayer
    case getPlayerMovement p of
        Nothing ->
            return ()
        Just m -> do
            let (xd, yd) = movementDisplacement m
            area <- getsGameState getArea
            modifyGameState $ \w ->
                w { getPlayer = p { getPlayerPosition = inBounds area (Position (x + xd, y + yd))
                                  , getPlayerMovement = Nothing } }
  where
    -- Force the coordinates back in the area's bounds.
    inBounds :: Area -> Position -> Position
    inBounds r@(Room {}) (Position (x, y)) =
        let (x1, y1, x2, y2) = getRoomBounds r in
        Position (max x1 (min x (x2 - 1)), max y1 (min y (y2 - 1)))

    -- How much does the player move for each movement command.
    movementDisplacement :: Direction -> (Int, Int)
    movementDisplacement North = (0, 1)
    movementDisplacement South = (0, -1)
    movementDisplacement West  = (-1, 0)
    movementDisplacement East  = (1, 0)

-- Move entities according the their own rules.
tickEntities :: Game State ()
tickEntities = do
    scheduleIn 2 tickEntities
    entities <- getsGameState getEntities
    entities' <- foldlM tickEntity entities (M.elems entities)
    modifyGameState (\w -> w { getEntities = entities' })

-- Move a single entity.  Zombies follow the player.  If a zombie tries to move to an
-- occupied space, it doesn't move.
tickEntity :: Map EntityId SomeEntity
           -> SomeEntity
           -> Game State (Map EntityId SomeEntity)
tickEntity entities _ = return entities

-- moveEntity w npcs z@(Zombie {}) =
  --   let (xz, yz) = getNpcPosition z
  --       Position (xp, yp) = getPlayerPosition (getPlayer w)
  --       pos' = if abs (xp - xz) > abs (yp - yz)
  --              then (xz + signum (xp - xz), yz)
  --              else (xz, yz + signum (yp - yz)) in
  --   if not (posOccupied pos' npcs)
  --   then M.insert (getEntityId z) (z { getNpcPosition = pos' }) npcs
  --   else npcs
  -- where
  --   posOccupied pos = M.foldl (\o npc -> o || getNpcPosition npc == pos) False

----------------------
-- Helpers
----------------------

-- | Schedule an action to run in the given number of ticks.
scheduleIn :: Int -> Game State () -> Game State ()
scheduleIn d act = do
    modifyGameState (\w ->
        w { getScheduler = scheduleAction (getScheduler w) (getTick w + d) act })

formatSeconds :: Double -> String
formatSeconds t = let secs = floor t :: Int
                      mins = secs `div` 60 in
                  printf "%02d:%02d.%d"
                         (mins `mod` 60)
                         (secs `mod` 60) ((floor ((t - fromIntegral secs) * 10.0) :: Int) `mod` 10)
