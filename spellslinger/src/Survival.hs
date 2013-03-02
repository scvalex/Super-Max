{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Survival (
        -- * State
        State, initState,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Common ( intRectangle, fromAreaCoordinates
              , bigText, mediumText )
import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Game.Engine ( GameEvent(..)
                   , Game, getsGameState, modifyGameState, getGameTick
                   , Picture(..)
                   , TextAlignment(..)
                   , Colour(..), black, greyN
                   , Event(..), SDLKey(..), Keysym(..) )
import Game.Entity ( Entity, Behaviour(..) )
import GlobalCommand ( GlobalCommand(..) )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Game.Entity as Entity
import qualified Zombie as Zombie
import RoomExit ( RoomExit, EntityParameters(..) )
import Spell ( )
import Text.Printf ( printf )
import Types ( Direction(..), EntityId(..), Position(..) )
import Zombie ( Zombie )

type Level = Int

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'handleTick').
data State =
    State { getState        :: RoundState
          , getLevel        :: Int
          , getTime         :: Double
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

data SomeEntity = forall a. (Entity a, Behaviour State a) => SomeEntity a

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
                 , getRoomStart   :: Position             -- ^ The player's starting point
                 , getRoomEntities :: forall a. [Game a SomeEntity] -- ^ The room's starting entities.
                 }

data Player = Player { getPlayerPosition :: Position
                     , getPlayerMovement :: Maybe Direction
                     } deriving ( Eq, Show )

----------------------
-- Callbacks
----------------------

area1 :: Area
area1 =
    let bounds = (0, 0, 60, 60) in
    let initExit = Entity.init (StaticExit { getAreaBounds = bounds
                                           , getAreaExitPosition = Position (29, 55) }) in
    Room { getRoomBounds = bounds
         , getRoomStart = Position (29, 5)
         , getRoomEntities = [SomeEntity <$> initExit]
         }

initState :: Level -> Game a State
initState lvl = do
    let area = area1
        roomBounds = getRoomBounds area
    entities <- foldlM (\ns _ -> do
                             z <- Entity.init (Zombie.RandomZombie
                                                   { Zombie.getAreaBounds = roomBounds
                                                   })
                             return (M.insert (Entity.eid z) (SomeEntity z) ns))
                       M.empty
                       [(1 :: Int) ..2^(lvl - 1)]
    roomEntities <- sequence (getRoomEntities area)
    let entities' = foldl (\m se@(SomeEntity e) -> M.insert (Entity.eid e) se m) entities roomEntities
    return (State { getState        = PreRound "Get to the exit"
                  , getLevel        = lvl
                  , getTime         = 0.0
                  , getArea         = area
                  , getHeldDownKeys = S.empty
                  , getEntities     = entities'
                  , getPlayer       = Player { getPlayerPosition = getRoomStart area
                                             , getPlayerMovement = Nothing
                                             }
                  })

drawState :: State -> Picture
drawState w =
    mconcat [ wireframe
            , player
            , entities
            , hud
            , prePostMessage
            ]
  where
    -- The wireframe in the background.
    wireframe = mappend (Colour (greyN 0.1) $
                         FilledRectangle 0.0 0.0 1.0 1.0) $
                Colour black $
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
        Colour (RGBA 255 140 0 255) $
        personPicture pos

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

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent (InputEvent ev) =
    handleInputEvent ev
handleEvent (Tick (_, t)) = do
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
                  , updateTime
                  , movePlayer
                  , tickEntities
                  ]

    -- Increment the ticker by the elapsed amount of time.
    updateTime :: Game State ()
    updateTime = do
        modifyGameState (\w -> w { getTime = getTime w + t })

    -- Some keys were held down, so we didn't see them "happen" this turn.  Simulate key
    -- presses for all keys that are currently being held down.
    processHeldDownKeys :: Game State ()
    processHeldDownKeys = do
        keys <- getsGameState getHeldDownKeys
        mapM_ processKey (S.toList keys)

----------------------
-- State updates
----------------------

-- Move the player according to its movement, then, reset its movement.
movePlayer :: Game State ()
movePlayer = do
    p <- getsGameState getPlayer
    let Position (x, y) = getPlayerPosition p
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

-- Tick entities according the their own rules.
tickEntities :: Game State ()
tickEntities = do
    entities <- getsGameState getEntities
    mapM_ tickEntity (M.elems entities)

-- Tick a single entity.
tickEntity :: SomeEntity -> Game State ()
tickEntity (SomeEntity e) = do
    e' <- behave e
    -- FIXME Add support for fps > tps.
    let e'' = Entity.tickVisual e'
    modifyGameState (\w ->
        w { getEntities = M.insert (Entity.eid e'') (SomeEntity e'') (getEntities w) })

----------------------
-- Entity behaviour
----------------------

-- Zombies follow the player.  If a zombie tries to move to an
-- occupied space, it doesn't move.
instance Behaviour State Zombie where
    behave z = do
        tick <- getGameTick
        if tick `mod` 2 == 0
            then moveZombie
            else return z
      where
        moveZombie = do
            let Position (xz, yz) = Zombie.getPosition z
            posp@(Position (xp, yp)) <- getPlayerPosition <$> getsGameState getPlayer
            let pos' = if abs (xp - xz) > abs (yp - yz)
                       then Position (xz + signum (xp - xz), yz)
                       else Position (xz, yz + signum (yp - yz))
            when (pos' == posp) $ do
                modifyGameState (\w -> w { getState = PostRound { getConclusion  = "You died"
                                                                , getCanContinue = False } })
            entities <- getsGameState getEntities
            if not (posOccupied pos' entities)
                then return (Zombie.setPosition z pos')
                else return z

        posOccupied pos =
            M.foldl (\o (SomeEntity e) -> o || pos `S.member` Entity.positions e) False

instance Behaviour State RoomExit where
    behave re = do
        posp <- getPlayerPosition <$> getsGameState getPlayer
        when (posp `S.member` Entity.positions re) $ do
            modifyGameState (\w -> w { getState = PostRound { getConclusion  = "You win"
                                                            , getCanContinue = True } })
        return re

----------------------
-- Helpers
----------------------

formatSeconds :: Double -> String
formatSeconds t = let secs = floor t :: Int
                      mins = secs `div` 60 in
                  printf "%02d:%02d.%d"
                         (mins `mod` 60)
                         (secs `mod` 60) ((floor ((t - fromIntegral secs) * 10.0) :: Int) `mod` 10)
