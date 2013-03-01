{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Engine (
        -- * The Game
        Game, GameEvent(..),

        -- * Pictures
        Picture(..),
        TextAlignment(..),
        Color(..), white, black, greyN,

        -- * SDL events (re-export)
        Event(..), SDLKey(..), Keysym(..),

        -- * Engine interface
        play, quitGame, getGameState, getsGameState, modifyGameState,
        withAlternateGameState, randomR, mkUid
    ) where

import Control.Applicative ( Applicative(..), (<$>) )
import Control.Concurrent ( threadDelay, forkIO, ThreadId )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, readTChan, writeTChan )
import Control.Exception ( Exception, assert )
import Control.Monad ( forever )
import Data.Map ( Map )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Data.Time.Clock ( UTCTime, getCurrentTime, diffUTCTime )
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
                       , Event(..), SDLKey(..), Keysym(..), waitEvent
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

--------------------------------
-- Missing instances
--------------------------------

deriving instance Ord Keysym

--------------------------------
-- Colors -- I want to call them Colours :(
--------------------------------

data Color = RGBA { colorRed   :: Word8
                  , colorGreen :: Word8
                  , colorBlue  :: Word8
                  , colorAlpha :: Word8
                  } deriving ( Eq )

instance Show Color where
    show c = printf "#%2x%2x%2x%2x" (colorRed c) (colorGreen c) (colorBlue c) (colorAlpha c)

white, black :: Color
white = RGBA 255 255 255 255
black = RGBA 0   0   0   255

greyN :: Double -> Color
greyN prop = RGBA sat sat sat 255
  where
    sat = floor (prop * 255.0)

-- | Convert a 'Color' to an SDL 'SDL.Color'.  Drops the alpha component.
colorToSdlColor :: Color -> SDL.Color
colorToSdlColor (RGBA r g b _) = SDL.Color r g b

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
             | Color Color Picture
             | Pictures [Picture]
             deriving ( Eq, Show )

instance Monoid Picture where
    mempty = Pictures []
    p1 `mappend` p2 = Pictures [p1, p2]
    mconcat ps = Pictures ps

--------------------------------
-- Game/Engine interface
--------------------------------

data EngineState s = EngineState { getInnerState :: s
                                 , getEventChan  :: TChan GameEvent
                                 , getThreads    :: Set ThreadId
                                 , isTerminating :: Bool
                                 , getGen        :: StdGen
                                 , getNextUid    :: Int
                                 }

-- | The events a game may receive.
data GameEvent = Tick Int (UTCTime, Double) -- ^ A logical tick with the current time and
                                            -- the number of seconds since the last tick.
               | InputEvent Event           -- ^ An input (mouse, keyboard, etc.) event

-- FIXME Just use a StateT for the game state.  Don't forget to use gets and sets in
-- blind-horror.hs.  | Psych!  It's a state monad!
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

-- | Run an action in the 'Game' monad but with a different game state.
withAlternateGameState :: t -> (t -> s) -> Game t a -> Game s a
withAlternateGameState alternateState setAlternateState innerAction =
    Game (\s -> let as = EngineState { getInnerState = alternateState
                                     , getEventChan  = getEventChan s
                                     , getThreads    = getThreads s
                                     , isTerminating = isTerminating s
                                     , getGen        = getGen s
                                     , getNextUid    = getNextUid s
                                     } in
                let (x, as') = runGame innerAction as in
                let s' = EngineState { getInnerState = setAlternateState (getInnerState as')
                                     , getEventChan  = getEventChan as'
                                     , getThreads    = getThreads as'
                                     , isTerminating = isTerminating as'
                                     , getGen        = getGen as'
                                     , getNextUid    = getNextUid as'
                                     } in
                (x, s'))

-- | Generate a random number using the game's random generator.
randomR :: (Random a) => (a, a) -> Game s a
randomR bounds = Game (\s -> let (x, g) = R.randomR bounds (getGen s) in (x, s { getGen = g }))

-- | Return a unique 'Int'.  This function never returns the same value twice.
mkUid :: Game s Int
mkUid = Game (\s -> let uid = getNextUid s in (uid, s { getNextUid = uid + 1 }))

--------------------------------
-- Fonts
--------------------------------

type Fonts = Map Int TTF.Font

--------------------------------
-- Drawing
--------------------------------

draw :: Surface -> Fonts -> Mat3 -> Color -> Picture -> IO ()
draw surface _ proj col (FilledRectangle x y w h) = do
    let pf = surfaceGetPixelFormat surface
    p <- mapRGBA pf (colorRed col) (colorGreen col) (colorBlue col) (colorAlpha col)
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
            textSurface <- TTF.renderUTF8Solid font text (colorToSdlColor col)
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
draw surface fonts proj _ (Color col picture) = do
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
        s <- setVideoMode screenW screenH 32 [HWSurface, Fullscreen]
        act screenW screenH s

--------------------------------
-- The Engine loop
--------------------------------

-- | 'Shutdown' is sent to each non-main game thread when the engine shuts down.
data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

play :: forall w.
        Int                         -- ^ Logical ticks per second
     -> (Int -> Int -> w)           -- ^ Take the screen width and height, and return the
                                    -- initial game state
     -> (w -> Picture)              -- ^ Draw a particular state
     -> (GameEvent -> Game w ())    -- ^ Update the state after a 'GameEvent'
     -> IO ()
play tps wInit drawGame onEvent = do
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

        let es = EngineState { getInnerState = wInit screenW screenH
                             , getEventChan  = eventCh
                             , getThreads    = S.empty
                             , isTerminating = False
                             , getGen        = gen
                             , getNextUid    = 1
                             }
        -- Set up the first tick.
        initTime <- getCurrentTime
        es' <- tick 0 initTime es

        -- Forward SDL event.
        es'' <- forwardEvents es'

        playLoop screen screenW screenH fonts es''
  where
    playLoop :: Surface -> Int -> Int -> Fonts -> EngineState w -> IO ()
    playLoop screen screenW screenH fonts es = do
        -- Block for next event.
        event <- atomically (readTChan (getEventChan es))

        -- Notify game of event.
        let ((), es') = runGame (onEvent event) es

        -- Draw the current state.
        draw screen fonts idmtx black $
            FilledRectangle 0 0 (fromIntegral screenW) (fromIntegral screenH)
        draw screen fonts idmtx white $
            -- The origin is in the bottom right corner
            Translate 0.0 (fromIntegral screenH) $
            Scale 1.0 (-1.0) $
            (drawGame (getInnerState es'))
        SDL.flip screen

        -- Set up the next tick.
        es'' <- case event of
            Tick cnt (prevTime, _) -> tick (cnt + 1) prevTime es'
            _                      -> return es'

        if isTerminating es''
            then shutdown es''
            else playLoop screen screenW screenH fonts es''

    tick :: Int -> UTCTime -> EngineState s -> IO (EngineState s)
    tick cnt prevTime es = managedForkIO es $ do
        now <- getCurrentTime
        let delta = fromRational (toRational (diffUTCTime now prevTime))
            desiredDelay = fromIntegral (1000000 `div` tps)
        threadDelay (floor (min desiredDelay (2.0 * desiredDelay - delta * 1000000.0)))
        atomically (writeTChan (getEventChan es) (Tick cnt (now, delta)))

    -- | Wait for SDL event and forward them to the event channel.
    forwardEvents :: EngineState s -> IO (EngineState s)
    forwardEvents es = managedForkIO es $ forever $ do
        event <- waitEvent
        atomically (writeTChan (getEventChan es) (InputEvent event))

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
