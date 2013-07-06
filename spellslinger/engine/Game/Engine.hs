{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Engine (
        -- * Sub-modules
        module Game.Input,

        -- * The Game
        Game,

        -- * Pictures
        Picture(..),
        TextAlignment(..),
        Colour(..), white, black, greyN,
        colourToHexString, colourFromHexString,

        -- * Engine interface
        play, quitGame, getGameState, getsGameState, modifyGameState,
        withAlternateGameState, randomR, mkUid, getGameTick, upon,
        getResourceDirectory, getResource
    ) where

import Control.Applicative ( Applicative(..), (<$>) )
import Control.Concurrent ( forkIO, ThreadId )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, tryReadTChan, writeTChan )
import Control.Exception ( Exception, assert )
import Control.Monad ( forever )
import Data.Char ( digitToInt )
import Data.Dynamic ( Dynamic )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Data.Time.Clock ( UTCTime, getCurrentTime, addUTCTime )
import Data.Traversable ( forM )
import Data.Typeable ( Typeable )
import Data.Word ( Word8 )
import Data.Vect.Double ( Mat3(..), Matrix(..), LeftModule(..), Vec3(..)
                        , MultSemiGroup(..) )
import Graphics.UI.SDL ( InitFlag(..), withInit
                       , Surface, SurfaceFlag(..), setVideoMode
                       , getVideoInfo, videoInfoWidth, videoInfoHeight
                       , Rect(..), mapRGBA
                       , surfaceGetPixelFormat, fillRect
                       , waitEvent
                       , blitSurface
                       , Rect(..), getClipRect )
import System.Environment ( getEnv )
import System.FilePath ( (</>) )
import System.Random ( Random, StdGen, newStdGen )
import Text.Printf ( printf )
import qualified Control.Exception as CE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import qualified System.Random as R

-- Re-exported modules
import Game.Input

--------------------------------
-- Missing instances
--------------------------------

deriving instance Ord Keysym

--------------------------------
-- Colours
--------------------------------

data Colour = RGBA { colourRed   :: Word8
                   , colourGreen :: Word8
                   , colourBlue  :: Word8
                   , colourAlpha :: Word8
                   } deriving ( Eq, Read, Show, Typeable )

white, black :: Colour
white = RGBA 255 255 255 255
black = RGBA 0   0   0   255

greyN :: Double -> Colour
greyN prop = RGBA sat sat sat 255
  where
    sat = floor (prop * 255.0)

-- | Convert a 'Colour' to an SDL 'SDL.Color'.  Drops the alpha component.
colourToSdlColor :: Colour -> SDL.Color
colourToSdlColor (RGBA r g b _) = SDL.Color r g b

-- | Convert a 'Colour' to a string like @#00ff00ff@.
colourToHexString :: Colour -> String
colourToHexString col = printf "#%02x%02x%02x%02x"
                               (colourRed col)
                               (colourGreen col)
                               (colourBlue col)
                               (colourAlpha col)

colourFromHexString :: String -> Maybe Colour
colourFromHexString ('#':r1:r2:g1:g2:b1:b2:_) =
    let r = digitToInt r1 * 16 + digitToInt r2
        g = digitToInt g1 * 16 + digitToInt g2
        b = digitToInt b1 * 16 + digitToInt b2 in
    Just (RGBA (fromIntegral r) (fromIntegral g) (fromIntegral b) 255)
colourFromHexString _col =
    Nothing


--------------------------------
-- Pictures
--------------------------------

-- | Are the coordinates given for the text the left edge, the center, or the right edge
-- of the text?
data TextAlignment = LeftAligned
                   | CenterAligned
                   | RightAligned
                   deriving ( Eq, Show )

-- | The 'Picture' returned by the callback to 'play' is drawn on the screen.  The origin
-- of the coordinate system is in the bottom-left corner, and axes are the usual Cartesian
-- ones.
data Picture = FilledRectangle Double Double Double Double
             | Translate Double Double Picture
               -- Do /not/ use 'Scale' to flip the screen.
             | Scale Double Double Picture
             | Text Int TextAlignment String
             | Colour Colour Picture
             | Pictures [Picture]
             deriving ( Eq, Show )

instance Monoid Picture where
    mempty = Pictures []
    p1 `mappend` p2 = Pictures [p1, p2]
    mconcat ps = Pictures ps

--------------------------------
-- Game/Engine interface
--------------------------------

data IOAction s = forall a. IOAction (IO a) (a -> Game s ())

data EngineState s = EngineState { getInnerState :: s
                                 , getEventChan  :: TChan (EngineEvent s)
                                 , getThreads    :: Set ThreadId
                                 , isTerminating :: Bool
                                 , getGen        :: StdGen
                                 , getNextUid    :: Int
                                 , getTick       :: Int
                                 , getQueuedIO   :: [IOAction s]
                                 , getResDir     :: FilePath
                                 , getRes        :: Map String Dynamic
                                 }

-- | The events the engine's asynchronous components may send/receive.
data EngineEvent s = GameInput InputEvent
                   | GameAction (Game s ())

-- | Psych!  It's (almost) a state monad!
newtype Game s a = Game { runGame :: EngineState s -> (a, EngineState s) }

instance Monad (Game s) where
    return x = Game (\s -> (x, s))
    (Game h) >>= f = Game (\s -> let (x, s') = h s in
                                 let (Game g) = f x in
                                 g s')

instance Functor (Game s) where
    f `fmap` (Game h) = Game (\s -> let (x, s') = h s in (f x, s'))

instance Applicative (Game s) where
    pure x = Game (\s -> (x, s))
    Game f <*> Game g = Game (\s -> let (h, s') = f s in
                                    let (x, s'') = g s' in
                                    (h x, s''))

-- | Get the current game state.
getGameState :: Game s s
getGameState = Game (\s -> (getInnerState s, s))

-- | Get an inner field of the game state.
getsGameState :: (s -> a) -> Game s a
getsGameState getField = Game (\s -> (getField (getInnerState s), s))

-- | Pass the current game state through the given function.
modifyGameState :: (s -> s) -> Game s ()
modifyGameState f = Game (\s -> ((), s { getInnerState = f (getInnerState s) }))

-- 'setGameState' is unsafe and should not be used; use 'modifyGameState' instead.

-- | Instruct the engine to close after the current callback returns.
quitGame :: Game s ()
quitGame = Game (\s -> ((), s { isTerminating = True }))

-- FIXME The half-functioning state thing in 'withAlternateGameState' is horrible.
-- | Run an action in the 'Game' monad but with a different game state.  If the alternate
-- state is 'Nothing', doesn't do anything.
withAlternateGameState :: forall s t a.
                          (s -> Maybe t)    -- ^ get alternate state
                       -> (t -> s)          -- ^ set alternate state
                       -> Game t (Maybe a)  -- ^ action with alternate state
                       -> Game s (Maybe a)
withAlternateGameState getAlternateState setAlternateState innerAction =
    Game (\s -> case getAlternateState (getInnerState s) of
                     Just alternateState ->
                         -- Note that the alternate engine state is *not* fully functional.
                         let as = EngineState { getInnerState = alternateState
                                              , getEventChan  = undefined
                                              , getThreads    = getThreads s
                                              , isTerminating = isTerminating s
                                              , getGen        = getGen s
                                              , getNextUid    = getNextUid s
                                              , getTick       = getTick s
                                              , getQueuedIO   = [] -- there's not point in
                                                                   -- passing them down
                                              , getResDir     = getResDir s
                                              , getRes        = getRes s
                                              } in
                         let (x, as') = runGame innerAction as in
                         -- FIXME Only change the parts of the state that should change.
                         let s' = EngineState { getInnerState = setAlternateState (getInnerState as')
                                              , getEventChan  = getEventChan s
                                              , getThreads    = getThreads as'
                                              , isTerminating = isTerminating as'
                                              , getGen        = getGen as'
                                              , getNextUid    = getNextUid as'
                                              , getTick       = getTick as'
                                              , getQueuedIO   = getQueuedIO s ++
                                                                mapToInnerState (getQueuedIO as')
                                              , getResDir     = getResDir as'
                                              , getRes        = getRes as'
                                              } in
                         (x, s')
                     Nothing -> (Nothing, s))
  where
    mapToInnerState :: [IOAction t] -> [IOAction s]
    mapToInnerState = map (\(IOAction act handler) -> IOAction act (toInnerState handler))

    toInnerState :: (b -> Game t ()) -> (b -> Game s ())
    toInnerState act = \x -> do
        _ <- withAlternateGameState
            getAlternateState
            setAlternateState
            (act x >> return Nothing)
        return ()

-- | Generate a random number using the game's random generator.
randomR :: (Random a) => (a, a) -> Game s a
randomR bounds = Game (\s -> let (x, g) = R.randomR bounds (getGen s) in (x, s { getGen = g }))

-- | Return a unique 'Int'.  This function never returns the same value twice.
mkUid :: Game s Int
mkUid = Game (\s -> let uid = getNextUid s in (uid, s { getNextUid = uid + 1 }))

-- | Get the current tick of the game.  This is the total number of 'Tick' events that
-- have taken place.
getGameTick :: Game s Int
getGameTick = Game (\s -> (getTick s, s))

-- | Run an 'IO' action on a separate thread, and then run the handler on the result.
-- Remember, there is only one game thread, so the handler will be run at some
-- indeterminate time in the future.
upon :: IO a -> (a -> Game s ()) -> Game s ()
upon act handler = Game (\s -> ((), s { getQueuedIO = IOAction act handler : getQueuedIO s }))

-- | Get the directory from where resources can be loaded.
getResourceDirectory :: Game s FilePath
getResourceDirectory = Game (\s -> (getResDir s, s))

-- | Get a named pre-loaded resource.
getResource :: String -> Game s (Maybe Dynamic)
getResource name = Game (\s -> (M.lookup name (getRes s), s))

--------------------------------
-- Fonts
--------------------------------

type Fonts = Map Int TTF.Font

--------------------------------
-- Drawing
--------------------------------

draw :: Surface -> Fonts -> Mat3 -> Colour -> Picture -> IO ()
draw surface _ proj col (FilledRectangle x y w h) = do
    let pf = surfaceGetPixelFormat surface
    p <- mapRGBA pf (colourRed col) (colourGreen col) (colourBlue col) (colourAlpha col)
    let (x1, y1) = projectXY proj x y
        (x2, y2) = projectXY proj (x + w) (y + h)
        (w', h') = (abs (x2 - x1), abs (y2 - y1))
        (x', y') = (min x1 x2, min y1 y2)
    let r = Rect { rectX = floor x', rectY = floor y'
                 , rectW = floor w', rectH = floor h' }
    ok <- fillRect surface (Just r) p
    assert ok (return ())
draw surface fonts proj col (Translate tx ty picture) = do
    let proj' = proj .*. (Mat3 (Vec3 1 0 tx) (Vec3 0 1 ty) (Vec3 0 0 1))
    draw surface fonts proj' col picture
draw surface fonts proj col (Scale sx sy picture) = do
    let proj' = proj .*. (Mat3 (Vec3 sx 0 0) (Vec3 0 sy 0) (Vec3 0 0 1))
    draw surface fonts proj' col picture
draw surface fonts proj col (Text size alignment text) = do
    case M.lookup size fonts of
        Nothing ->
            return ()
        Just font -> do
            -- FIXME Do we need to manually free used surfaces?
            textSurface <- TTF.renderUTF8Solid font text (colourToSdlColor col)
            textR <- getClipRect textSurface
            let (x, y) = projectXY proj 0 0
                x' = case alignment of
                    LeftAligned   -> floor x
                    RightAligned  -> floor x - rectW textR
                    CenterAligned -> floor x - (rectW textR `div` 2)
            ok <- blitSurface textSurface Nothing surface
                      (Just (textR { rectX = x'
                                   , rectY = floor y - rectH textR }))
            assert ok (return ())
draw surface fonts proj _ (Colour col picture) = do
    draw surface fonts proj col picture
draw surface fonts proj col (Pictures ps) = do
    mapM_ (draw surface fonts proj col) ps

-- | Run a pair of xy-coordinates through a projection matrix.
projectXY :: Mat3 -> Double -> Double -> (Double, Double)
projectXY proj x y =
    let Vec3 x' y' _ = proj *. (Vec3 x y 1)
    in (x', y')

--------------------------------
-- SDL initialization
--------------------------------

withScreen :: (Int -> Int -> Surface -> IO ()) -- ^ action to run
           -> IO ()
withScreen act = do
    withInit [InitEverything] $ do
        ok <- TTF.init
        assert ok (return ())
        vi <- getVideoInfo
        let screenW = videoInfoWidth vi
            screenH = videoInfoHeight vi
        s <- setVideoMode screenW screenH 32 [HWSurface]
        act screenW screenH s

--------------------------------
-- The Engine loop
--------------------------------

-- | 'Shutdown' is sent to each non-main game thread when the engine shuts down.
data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

play :: forall w.
        Int                                   -- ^ Logical ticks per second
     -> (FilePath -> IO (Map String Dynamic)) -- ^ Resource pre-loader.
     -> (Int -> Int -> w)                     -- ^ Take the screen width and height, and return the
                                              -- initial game state
     -> Game w ()                             -- ^ An initialization action.
     -> (w -> Picture)                        -- ^ Draw a particular state
     -> (InputEvent -> Game w ())             -- ^ Update the state after an 'InputEvent'
     -> (Float -> Game w ())                  -- ^ Update the state after a tick
     -> IO ()
play tps loadResources wInit start drawGame onInput onTick = do
    withScreen $ \screenW screenH screen -> do
        putStrLn "SDL initialised"

        -- Load resources
        spellslingerDir <- CE.handle (\(_ :: CE.SomeException) -> return Nothing) $
                             Just <$> getEnv "SPELLSLINGER_DIR"
        let resDir = maybe "r" (</> "r") spellslingerDir
        _ <- printf "Loading resources from %s\n" resDir

        -- We don't bother freeing the fonts.
        -- FIXME Let the game specify which fonts to load
        fonts <- M.fromList <$> forM [10..80] (\size -> do
            font <- TTF.openFont (resDir </> "Ubuntu-C.ttf") size
            return (size, font))

        putStrLn "Resources loaded"

        -- This channel is used to collect events from multiple sources for the game.
        eventCh <- newTChanIO

        -- The random number generator used throughout the game.
        gen <- newStdGen

        -- Pre-load resources
        resources <- loadResources resDir

        let es = EngineState { getInnerState = wInit screenW screenH
                             , getEventChan  = eventCh
                             , getThreads    = S.empty
                             , isTerminating = False
                             , getGen        = gen
                             , getNextUid    = 1
                             , getTick       = 0
                             , getQueuedIO   = []
                             , getResDir     = resDir
                             , getRes        = resources
                             }

        -- Forward SDL event.
        es' <- forwardEvents es

        -- Start the game
        let ((), es'') = runGame start es'

        fixedSliceLoop (1.0 / fromIntegral tps) screen screenW screenH fonts es''
  where
    -- | A fixed slice loop.  See this article for details:
    -- http://fabiensanglard.net/timer_and_framerate/index.php
    fixedSliceLoop :: Float          -- ^ Slice in s
                   -> Surface        -- ^ Screen surface
                   -> Int            -- ^ Screen width
                   -> Int            -- ^ Screen height
                   -> Fonts          -- ^ Fonts map
                   -> EngineState w  -- ^ Game state
                   -> IO ()
    fixedSliceLoop slice screen screenW screenH fonts es0 = do
        now <- getCurrentTime
        fixedSliceLoop' now es0
      where
        fixedSliceLoop' simulationTime1 es1 = do
            now <- getCurrentTime
            (simulationTime2, es2) <- updateUntilSimulationInTheFuture now simulationTime1 es1

            -- Draw the current state.
            draw screen fonts idmtx black $
                FilledRectangle 0 0 (fromIntegral screenW) (fromIntegral screenH)
            draw screen fonts idmtx white $
                -- The origin is in the bottom right corner
                Translate 0.0 (fromIntegral screenH) $
                Scale 1.0 (-1.0) $
                (drawGame (getInnerState es2))
            SDL.flip screen

            -- Start queued IO actions
            es3 <- startQueuedIO es2

            if isTerminating es3
                then shutdown es3
                else fixedSliceLoop' simulationTime2 es3

        updateUntilSimulationInTheFuture :: UTCTime
                                         -> UTCTime
                                         -> EngineState w
                                         -> IO (UTCTime, EngineState w)
        updateUntilSimulationInTheFuture now simulationTime1 es1 = do
            if simulationTime1 < now
                then do
                    es2 <- processEvents es1
                    let ((), es3) = runGame (onTick slice) es2  -- Fixed slice
                    let simulationTime2 = addUTCTime (fromRational (toRational slice))
                                                     simulationTime1
                    return (simulationTime2, es3)
                else do
                    return (simulationTime1, es1)

        processEvents :: EngineState w -> IO (EngineState w)
        processEvents es1 = do
            -- Get next event, if any
            mevent <- atomically $ tryReadTChan (getEventChan es1)

            case mevent of
                Nothing -> do
                    return es1
                Just event -> do
                    -- Notify game of event.
                    let ((), es2) = case event of
                            GameInput gevent -> runGame (onInput gevent) es1
                            GameAction gact  -> runGame gact es1
                    return es2

    startQueuedIO :: EngineState w -> IO (EngineState w)
    startQueuedIO (es@EngineState { getQueuedIO = ioacts }) = do
        es' <- foldlM startIOAction es ioacts
        return (es' { getQueuedIO = [] })
      where
        startIOAction es0 (IOAction act handler) =
            managedForkIO es0 $ do
                x <- act
                atomically (writeTChan (getEventChan es0) (GameAction (handler x)))

    -- | Wait for SDL event and forward them to the event channel.
    forwardEvents :: EngineState s -> IO (EngineState s)
    forwardEvents es = managedForkIO es $ forever $ do
        event <- waitEvent
        atomically (writeTChan (getEventChan es) (GameInput (InputEvent event)))

    -- | Fork a thread and keep track of its id.
    managedForkIO :: EngineState s -> IO () -> IO (EngineState s)
    managedForkIO es act = do
        tid <- forkIO (CE.handle (\(_ :: Shutdown) -> return ()) act)
        -- FIXME Memory leak: thread ids are never removed from the set.
        return es { getThreads = S.insert tid (getThreads es) }

    -- | Shutdown the engine by throwing 'Shutdown' to all its non-main threads.
    shutdown :: EngineState s -> IO ()
    shutdown es = do
        _ <- forM (S.toList (getThreads es)) (\tid -> CE.throwTo tid Shutdown)
        putStrLn "Shutdown"
