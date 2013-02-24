{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ( flip )

import Control.Applicative ( Applicative(..) )
import Control.Concurrent ( threadDelay, forkIO, ThreadId )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, readTChan, writeTChan )
import Control.Exception ( Exception, assert )
import Control.Monad ( forever )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Data.Traversable ( forM )
import Data.Typeable ( Typeable )
import Data.Word ( Word8 )
import Data.Vect.Double ( Mat3(..), Matrix(..), LeftModule(..), Vec3(..)
                        , MultSemiGroup(..) )
import Graphics.UI.SDL ( InitFlag(..), withInit
                       , Surface, SurfaceFlag(..), setVideoMode, flip
                       , Rect(..), mapRGBA
                       , surfaceGetPixelFormat, fillRect
                       , Event(..), SDLKey(..), Keysym(..), waitEvent
                       , blitSurface
                       , Rect(..), getClipRect )
import Text.Printf ( printf )
import qualified Control.Exception as CE
import qualified Data.Set as S
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

--------------------------------
-- Colors -- I want to call them Colours :(
--------------------------------

data Color = Color { colorRed   :: Word8
                   , colorGreen :: Word8
                   , colorBlue  :: Word8
                   , colorAlpha :: Word8
                   } deriving ( Eq )

instance Show Color where
    show c = printf "#%2x%2x%2x%2x" (colorRed c) (colorGreen c) (colorBlue c) (colorAlpha c)

--------------------------------
-- Pictures and drawing
--------------------------------

data Picture = FilledRectangle Double Double Double Double Color
             | Translate Double Double Picture
             | Scale Double Double Picture
             | AbsSizedText String
             | Pictures [Picture]
             deriving ( Eq, Show )

instance Monoid Picture where
    mempty = Pictures []
    p1 `mappend` p2 = Pictures [p1, p2]
    mconcat ps = Pictures ps

draw :: Surface -> TTF.Font -> Mat3 -> Picture -> IO ()
draw surface _ proj (FilledRectangle x y w h c) = do
    let pf = surfaceGetPixelFormat surface
    p <- mapRGBA pf (colorRed c) (colorGreen c) (colorBlue c) (colorAlpha c)
    let (x1, y1) = projectXY proj x y
        (x2, y2) = projectXY proj (x + w) (y + h)
        (w1, h1) = (x2 - x1, y2 - y1)
    let r = Rect { rectX = floor x1, rectY = floor y1
                 , rectW = floor w1, rectH = floor h1 }
    ok <- fillRect surface (Just r) p
    assert ok (return ())
draw surface font proj (Translate tx ty picture) = do
    let proj' = proj .*. (Mat3 (Vec3 1 0 tx) (Vec3 0 1 ty) (Vec3 0 0 1))
    draw surface font proj' picture
draw surface font proj (Scale sx sy picture) = do
    let proj' = proj .*. (Mat3 (Vec3 sx 0 0) (Vec3 0 sy 0) (Vec3 0 0 1))
    draw surface font proj' picture
draw surface font proj (AbsSizedText text) = do
    -- FIXME Do we need to manually free used surfaces?
    textSurface <- TTF.renderUTF8Solid font text (SDL.Color 255 255 255)
    textR <- getClipRect textSurface
    let (x, y) = projectXY proj 0 0
    ok <- blitSurface textSurface Nothing surface (Just (textR { rectX = floor x
                                                               , rectY = floor y }))
    assert ok (return ())
draw surface font proj (Pictures ps) = do
    mapM_ (draw surface font proj) ps

-- | Run a pair of xy-coordinates through a projection matrix.
projectXY :: Mat3 -> Double -> Double -> (Double, Double)
projectXY proj x y =
    let Vec3 x' y' _ = proj *. (Vec3 x y 1)
    in (x', y')

--------------------------------
-- SDL initialization
--------------------------------

withScreen :: Int                -- ^ width
           -> Int                -- ^ height
           -> (Surface -> IO ()) -- ^ action to run
           -> IO ()
withScreen w h act = do
    withInit [InitEverything] $ do
        ok <- TTF.init
        assert ok (return ())
        s <- setVideoMode w h 32 [SWSurface]
        act s

--------------------------------
-- Game/Engine interface
--------------------------------

data EngineState s = EngineState { getInnerState :: s
                                 , getEventChan  :: TChan GameEvent
                                 , getThreads    :: Set ThreadId
                                 , isTerminating :: Bool
                                 }

-- | Psych!  It's a state monad!
newtype Game s a = Game { runGame :: EngineState s -> (a, EngineState s) }

instance Monad (Game s) where
    return x = Game (\s -> (x, s))
    (Game h) >>= f = Game (\s -> let (x, s') = h s in let (Game g) = f x in g s')

instance Functor (Game s) where
    f `fmap` (Game h) = Game (\s -> let (x, s') = h s in (f x, s'))

instance Applicative (Game s) where
    pure x = Game (\s -> (x, s))
    Game f <*> Game g = Game (\s -> let (h, s') = f s in let (x, s'') = g s' in (h x, s''))

-- | Get the current game state.
getGameState :: Game s s
getGameState = Game (\s -> (getInnerState s, s))

-- | Overwrite the current game state.
setGameState :: s -> Game s ()
setGameState is = Game (\s -> ((), s { getInnerState = is }))

-- | Pass the current game state through the given function.
modifyGameState :: (s -> s) -> Game s ()
modifyGameState f = Game (\s -> ((), s { getInnerState = f (getInnerState s) }))

-- | Instruct the engine to close after the current callback returns.
quitGame :: Game s ()
quitGame = Game (\s -> ((), s { isTerminating = True }))

-- | The events a game may receive.
data GameEvent = Tick Double      -- ^ A logical tick with the number of seconds since the
                                  -- last one.
               | InputEvent Event -- ^ An input (mouse, keyboard, etc.) event

play :: forall w.
        (Int, Int)                  -- ^ width, height of the game window
     -> Int                         -- ^ Logical ticks per second
     -> w                           -- ^ Game state
     -> (w -> Picture)              -- ^ Draw a particular state
     -> (GameEvent -> Game w ())    -- ^ Update the state after a 'GameEvent'
     -> IO ()
play (screenW, screenH) tps wInit drawGame onEvent = do
    withScreen screenW screenH $ \screen -> do
        putStrLn "SDL initialised"

        -- Load resources
        -- We don't bother freeing the font.
        font <- TTF.openFont "r/Ubuntu-C.ttf" 28

        putStrLn "Resources loaded"

        -- This channel is used to collect events from multiple sources for the game.
        eventCh <- newTChanIO

        let es = EngineState { getInnerState = wInit
                             , getEventChan  = eventCh
                             , getThreads    = S.empty
                             , isTerminating = False
                             }
        -- Set up the first tick.
        es' <- tick es

        -- Forward SDL event.
        es'' <- forwardEvents es'

        playLoop screen font es''
  where
    playLoop :: Surface -> TTF.Font -> EngineState w -> IO ()
    playLoop screen font es = do
        -- Block for next event.
        event <- atomically (readTChan (getEventChan es))

        -- Notify game of event.
        let ((), es') = runGame (onEvent event) es

        -- Draw the current state.
        draw screen font idmtx $
            FilledRectangle 0 0 (fromIntegral screenW) (fromIntegral screenH) (Color 0 0 0 255)
        draw screen font idmtx (drawGame (getInnerState es'))
        flip screen

        -- Set up the next tick.
        es'' <- case event of
            Tick _ -> tick es'
            _      -> return es'

        if isTerminating es''
            then shutdown es''
            else playLoop screen font es''

    -- FIXME Actually tick at tps (not at slightly less).
    tick :: EngineState s -> IO (EngineState s)
    tick es = managedForkIO es $ do
        threadDelay (1000000 `div` tps)
        -- FIXME Pass in the real time to Tick.
        atomically (writeTChan (getEventChan es) (Tick undefined))

    -- | Wait for SDL event and forward them to the event channel.
    forwardEvents :: EngineState s -> IO (EngineState s)
    forwardEvents es = managedForkIO es $ forever $ do
        event <- waitEvent
        atomically (writeTChan (getEventChan es) (InputEvent event))

    -- | Fork a thread and keep track of its id.
    managedForkIO :: EngineState s -> IO () -> IO (EngineState s)
    managedForkIO es act = do
        tid <- forkIO (CE.handle (\(_ :: Shutdown) -> return ()) act)
        return es { getThreads = S.insert tid (getThreads es) }

    -- | Shutdown the engine by throwing 'Shutdown' to all its non-main threads.
    shutdown :: EngineState s -> IO ()
    shutdown es = do
        _ <- forM (S.toList (getThreads es)) (\tid -> CE.throwTo tid Shutdown)
        return ()

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

--------------------------------
-- Runner
--------------------------------

main :: IO ()
main = do
    play (640, 480) 10 (0 :: Int) drawRects handleEvent
  where
    drawRects i =
        mconcat [ Translate (10 * fromIntegral (i `mod` 40)) 50 $
                  FilledRectangle 0 0 10 10 (Color 255 0 0 255)
                , Translate 30 (10 * fromIntegral (i `mod` 30)) $
                  FilledRectangle 0 0 10 10 (Color 0 255 0 255)
                , Translate 100 100 $
                  AbsSizedText "Test"
                ]

    handleEvent (Tick _) = do
        modifyGameState (+1)
    handleEvent (InputEvent (KeyUp (Keysym {symKey = SDLK_ESCAPE}))) =
        quitGame
    handleEvent _ = do
        return ()
