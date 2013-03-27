{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Survival (
        -- * State
        State, initState, start,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Common ( intRectangle, fromAreaCoordinates
              , bigText, mediumText
              , writeAppFile, readAppFile )
import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Entities.InvisibleWall ( InvisibleWall, EntityParameters(..) )
import Entities.Notice ( Notice, EntityParameters(..) )
import Entities.RoomExit ( RoomExit, EntityParameters(..) )
import Entities.Zombie ( Zombie )
import Game.Engine ( GameEvent(..)
                   , Game, getsGameState, modifyGameState, getGameTick, upon, randomR
                   , Picture(..)
                   , TextAlignment(..)
                   , Colour(..), black, greyN, colourToHexString
                   , Event(..), SDLKey(..), Keysym(..) )
import Game.Entity ( Entity, Behaviour(..), EntityId(..), Position(..) )
import GlobalCommand ( GlobalCommand(..) )
import Profile ( Profile(..), loadOrNewProfile )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Entities.InvisibleWall as InvisibleWall
import qualified Entities.Notice as Notice
import qualified Entities.RoomExit as RoomExit
import qualified Entities.Zombie as Zombie
import qualified Game.Entity as Entity
import Network.HTTP ( Request, RequestMethod(..), mkRequest, simpleHTTP )
import Network.URI ( parseURI )
import Spell ( )
import Text.Printf ( printf )
import Types ( Score, Direction(..), randomDirection )

-- | The level is just an incrementing integer.
type Level = Int

-- | The state of the world is used to generate the scene, and is
-- updated on every event (see 'handleEvent'), and on every tick (see
-- 'handleTick').
data State =
    State { getState        :: RoundState
          , getLevel        :: Maybe Level
          , getScore        :: Score
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
          , getPlayerColour :: Colour
          }

data SomeEntity = forall a. (Entity a, Behaviour State a) => SomeEntity a

data FurtherOptions = Restart | Continue
                    deriving ( Eq, Show )

data RoundState = Loading
                | PreRound { getObjective :: String }
                | InRound
                | PostRound { getConclusion     :: String
                            , getFurtherOptions :: FurtherOptions }
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
    let bounds = (0, 0, 59, 59) in
    let initExit = Entity.init (StaticExit { RoomExit.getAreaBounds = bounds
                                           , getAreaExitPosition = Position (29, 55) }) in
    let initNotice = Entity.init (RandomNotice { Notice.getAreaBounds = bounds
                                               , getStaticPosition = Position ( 29, 30 )
                                               , getPossibleTexts =
                                                   [ "ProTip: Run from the green things."
                                                   , "Green person wants a hug!"
                                                   , "CUT OFF THEIR LIMBS"
                                                   , "Dead girls *can* say no."
                                                   , "Q: What's funnier than a dead baby?"
                                                   , "A: A dead baby mauling your face."
                                                   ]
                                               }) in
    let initWalls = Entity.init (BorderWall { InvisibleWall.getAreaBounds = bounds }) in
    Room { getRoomBounds = bounds
         , getRoomStart = Position (29, 5)
         , getRoomEntities = [ SomeEntity <$> initExit
                             , SomeEntity <$> initNotice
                             , SomeEntity <$> initWalls
                             ]
         }

initState :: State
initState =
    State { getState        = Loading
          , getLevel        = Nothing
          , getScore        = 0
          , getTime         = 0.0
          , getArea         = area1
          , getHeldDownKeys = S.empty
          , getEntities     = M.empty
          , getPlayer       = Player { getPlayerPosition = getRoomStart area1
                                     , getPlayerMovement = Nothing
                                     }
          , getPlayerColour = RGBA 255 140 0 255
          }

start :: Maybe (Level, Score) -> Game State ()
start Nothing =
    loadOrNewProfile `upon` \profile -> do
        modifyGameState (\w -> w { getPlayerColour = getProfilePlayerColour profile })
        readAppFile "lastLevel" `upon` \mtext -> do
            let (lvl, score) = maybe (1, 0) read mtext
            modifyGameState (\w -> w { getScore = score })
            loadLevel lvl
start (Just (lvl, score)) =
    loadOrNewProfile `upon` \profile -> do
        modifyGameState (\w -> w { getPlayerColour = getProfilePlayerColour profile
                                 , getScore        = score })
        loadLevel lvl

loadLevel :: Level -> Game State ()
loadLevel lvl = do
    let roomBounds = getRoomBounds area1
    zombies <- foldlM (\ns _ -> do
                            -- FIXME Zombies may spawn on top of player.  And on top of filled entities.
                            z <- Entity.init (Zombie.RandomZombie
                                                   { Zombie.getAreaBounds = roomBounds
                                                   })
                            return (M.insert (Entity.eid z) (SomeEntity z) ns))
                      M.empty
                      [(1 :: Int) ..2^(lvl - 1)]
    roomEntities <- sequence (getRoomEntities area1)
    let entities = foldl (\m se@(SomeEntity e) -> M.insert (Entity.eid e) se m) zombies roomEntities
    modifyGameState (\w ->
        w { getState        = PreRound "Get to the exit"
          , getLevel        = Just lvl
          , getTime         = 0.0
          , getHeldDownKeys = S.empty
          , getEntities     = entities
          , getPlayer       = Player { getPlayerPosition = getRoomStart area1
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
            Loading ->
                Translate 0.5 0.45 $ bigText CenterAligned "Loading..."
            pg@(PreRound {}) ->
                mconcat [ Translate 0.5 0.45 $ bigText CenterAligned (getObjective pg)
                        , Translate 0.5 0.40 $ mediumText CenterAligned "<press space to start>"
                        ]
            InRound {} ->
                mempty
            pg@(PostRound { getFurtherOptions = opts }) ->
                mconcat [ Translate 0.5 0.45 $ bigText CenterAligned (getConclusion pg)
                        , Translate 0.5 0.40 $ mediumText CenterAligned $
                          case opts of
                              Continue -> "<press space for next level>"
                              Restart  -> "<press space to try again>"
                        ]

    -- FIXME The order of entities is not well-defined (zombies can be hidden by the room exit).
    -- All the non-player entities.
    entities =
        mconcat $
        map (\(SomeEntity e) -> Entity.draw e) (M.elems (getEntities w))

    -- The player.
    player =
        fromRoomCoordinates $
        let pos = getPlayerPosition (getPlayer w) in
        Colour (getPlayerColour w) $
        personPicture pos

    -- The HUD is overlayed on the game.
    hud = mconcat [ survivalTime
                  , score
                  , currentLevel
                  ]

    -- Survival time in top-left corner.
    survivalTime = Translate 0.04 0.94 $
                   bigText LeftAligned (formatSeconds (getTime w))

    -- Current score in the top-left corner.
    score = Translate 0.04 0.91 $
            mediumText LeftAligned (printf "Score: %d" (getScore w))

    -- Current level in the top-left corner.
    currentLevel = Translate 0.04 0.88 $
                   mediumText LeftAligned (printf "Level: %s" (maybe "?" show (getLevel w)))

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
        Loading ->
            return ()
        PreRound {}  -> do
            case ev of
                KeyUp (Keysym { symKey = SDLK_SPACE }) -> do
                    modifyGameState (\w' -> w' { getState = InRound })
                _ -> do
                    handleInRoundEvent
        InRound -> do
            handleInRoundEvent
        PostRound { getFurtherOptions = opts } -> do
            case ev of
                KeyUp (Keysym { symKey = SDLK_SPACE })-> do
                    case opts of
                        Continue -> do
                            Just lvl <- getsGameState getLevel
                            loadLevel (lvl + 1)
                        Restart -> do
                            Just lvl <- getsGameState getLevel
                            loadLevel lvl
                _ -> do
                    return ()
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
        -- We enumerate all the cases here so that, when we add a new one, we don't forget
        -- about this.
        Loading      -> return ()
        PreRound {}  -> return ()
        InRound      -> handleTickInRound
        PostRound {} -> return ()
  where
    handleTickInRound :: Game State ()
    handleTickInRound =
        sequence_ [ processHeldDownKeys
                  , updateTime
                  , tickEntities
                    -- FIXME Because the player moves *after* entities, it looks like they escaped when they didn't.
                  , movePlayer
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
                pos' = Position (x + xd, y + yd)
            entities <- getsGameState getEntities
            if posOccupied pos' entities
                then do
                    modifyGameState $ \w ->
                        w { getPlayer = p { getPlayerMovement = Nothing } }
                else do
                    modifyGameState $ \w ->
                        w { getPlayer = p { getPlayerPosition = pos'
                                          , getPlayerMovement = Nothing } }

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

-- FIXME The split between Behaviour and Entity is a bit weird.
-- Zombies follow the player.  If a zombie tries to move to an
-- occupied space, it doesn't move.
instance Behaviour State Zombie where
    behave zombie = do
        tick <- getGameTick

        -- | Zombies follow the player if he moves and is close enough.
        pmovement <- getPlayerMovement <$> getsGameState getPlayer
        Position (xp, yp) <- getPlayerPosition <$> getsGameState getPlayer
        let Position (xz, yz) = Zombie.getPosition zombie
        let closeEnough = abs (xp - xz) <= 18 && abs (yp - yz) <= 18
        let z = if pmovement /= Nothing && closeEnough
                then Zombie.alertZombie zombie
                else zombie

        case Zombie.getState z of
            Zombie.Following | tick `mod` 2 == 0 ->
                moveZombieFollowing z
            Zombie.Roaming dir | tick `mod` 4 == 0 ->
                moveZombieRoaming z dir
            _ ->
                return zombie
      where
        moveZombieFollowing z = do
            let Position (xz, yz) = Zombie.getPosition z
            posp@(Position (xp, yp)) <- getPlayerPosition <$> getsGameState getPlayer
            let pos' = if abs (xp - xz) > abs (yp - yz)
                       then Position (xz + signum (xp - xz), yz)
                       else Position (xz, yz + signum (yp - yz))
            when (pos' == posp) roundLost
            entities <- getsGameState getEntities
            let cannotMove = posOccupied pos' entities
            if cannotMove
                then return z
                else return (Zombie.setPosition z pos')

        moveZombieRoaming z dir = do
            let Position (xz, yz) = Zombie.getPosition z
                (xd, yd) = movementDisplacement dir
                pos' = Position (xz + xd, yz + yd)
            posp <- getPlayerPosition <$> getsGameState getPlayer
            when (pos' == posp) roundLost
            entities <- getsGameState getEntities
            n <- randomR (1, 10 :: Int)
            let cannotMove = posOccupied pos' entities
            z' <- if n == 1 || cannotMove
                  then Zombie.roamTowards z <$> randomDirection
                  else return z
            if cannotMove
                then return z'
                else return (Zombie.setPosition z' pos')

-- Stepping onto the room exit wins you the round.
instance Behaviour State RoomExit where
    behave re = do
        posp <- getPlayerPosition <$> getsGameState getPlayer
        when (RoomExit.contains re posp) roundWon
        return re

instance Behaviour State Notice where
    behave n = do
        Position (xp, yp) <- getPlayerPosition <$> getsGameState getPlayer
        let Position (xn, yn) = Notice.getPosition n
        if abs (xp - xn) + abs (yp - yn) <= 2
            then return (Notice.activated n)
            else return (Notice.deactivated n)

-- "The walls are just, like there, man.  They don't do stuff."
instance Behaviour State InvisibleWall where
    behave iw = do
        return iw

----------------------
-- Helpers
----------------------

formatSeconds :: Double -> String
formatSeconds t = let secs = floor t :: Int
                      mins = secs `div` 60 in
                  printf "%02d:%02d.%d"
                         (mins `mod` 60)
                         (secs `mod` 60) ((floor ((t - fromIntegral secs) * 10.0) :: Int) `mod` 10)

-- | Switch the state to post-round win.
roundWon :: Game State ()
roundWon = do
    Just lvl <- getsGameState getLevel
    let scoreIncrement = 2 ^ lvl
    modifyGameState (\w -> w { getScore = getScore w + scoreIncrement })
    newScore <- getsGameState getScore
    submitScore newScore `upon` \() -> return ()
    score <- getsGameState getScore
    writeAppFile "lastLevel" (show (lvl + 1, score))
        `upon` (\_ -> return ())
    modifyGameState (\w -> w { getState = PostRound { getConclusion     = "You win"
                                                    , getFurtherOptions = Continue } })
  where
    submitScore score = do
        profile <- loadOrNewProfile
        let stringUri = printf "http://abstractbinary.org/spellslinger/add-score/%s/%s/%d"
                               (getProfilePlayerName profile)
                               (tail (colourToHexString (getProfilePlayerColour profile)))
                               score
            Just uri = parseURI stringUri
            req = mkRequest PUT uri :: Request String
        _ <- Network.HTTP.simpleHTTP req
        return ()

-- | Switch the state to post-round loss.
roundLost :: Game State ()
roundLost =
    modifyGameState (\w -> w { getState = PostRound { getConclusion     = "You died"
                                                    , getFurtherOptions = Restart } })

-- | Is the given position occupied by some entity?
posOccupied :: Position -> Map EntityId SomeEntity -> Bool
posOccupied pos =
    M.foldl (\o (SomeEntity e) -> o || pos `S.member` Entity.occupiedPositions e) False

-- | How much does something move for each direction.
movementDisplacement :: Direction -> (Int, Int)
movementDisplacement North = (0, 1)
movementDisplacement South = (0, -1)
movementDisplacement West  = (-1, 0)
movementDisplacement East  = (1, 0)
