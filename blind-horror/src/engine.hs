{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ( flip )

import Control.Applicative ( Applicative(..) )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, readTChan, writeTChan )
import Control.Exception ( assert )
import Control.Monad ( void, forever )
import Data.Monoid ( Monoid(..) )
import Data.Word ( Word8 )
import Data.Vect.Double ( Mat3(..), Matrix(..), LeftModule(..), Vec3(..)
                        , MultSemiGroup(..) )
import Graphics.UI.SDL ( InitFlag(..), withInit
                       , Surface, SurfaceFlag(..), setVideoMode, flip
                       , Rect(..), mapRGBA
                       , surfaceGetPixelFormat, fillRect
                       , Event(..), SDLKey(..), Keysym(..), waitEvent )
import Text.Printf ( printf )

--------------------------------
-- Colors -- I want to call them Colours :(
--------------------------------

data Color = Color { colorRed   :: Word8
                   , colorGreen :: Word8
                   , colorBlue  :: Word8
                   , colorAlpha :: Word8
                   } deriving (Eq)

instance Show Color where
    show c = printf "#%2x%2x%2x%2x" (colorRed c) (colorGreen c) (colorBlue c) (colorAlpha c)

--------------------------------
-- Pictures and drawing
--------------------------------

data Picture = FilledRectangle Double Double Double Double Color
             | Translate Double Double Picture
             | Scale Double Double Picture
             | Pictures [Picture]
             deriving ( Eq, Show )

instance Monoid Picture where
    mempty = Pictures []
    p1 `mappend` p2 = Pictures [p1, p2]
    mconcat ps = Pictures ps

draw :: Surface -> Mat3 -> Picture -> IO ()
draw surface proj (FilledRectangle x y w h c) = do
    let pf = surfaceGetPixelFormat surface
    p <- mapRGBA pf (colorRed c) (colorGreen c) (colorBlue c) (colorAlpha c)
    let Vec3 x1 y1 _ = proj *. (Vec3 x y 1)
        Vec3 x2 y2 _ = proj *. (Vec3 (x + w) (y + h) 1)
        (w1, h1) = (x2 - x1, y2 - y1)
    let r = Rect { rectX = floor x1, rectY = floor y1
                 , rectW = floor w1, rectH = floor h1 }
    ok <- fillRect surface (Just r) p
    assert ok (return ())
draw surface proj (Translate tx ty picture) = do
    let proj' = proj .*. (Mat3 (Vec3 1 0 tx) (Vec3 0 1 ty) (Vec3 0 0 1))
    draw surface proj' picture
draw surface proj (Scale sx sy picture) = do
    let proj' = proj .*. (Mat3 (Vec3 sx 0 0) (Vec3 0 sy 0) (Vec3 0 0 1))
    draw surface proj' picture
draw surface proj (Pictures ps) = do
    mapM_ (draw surface proj) ps

--------------------------------
-- SDL initialization
--------------------------------

withScreen :: Int                -- ^ width
           -> Int                -- ^ height
           -> (Surface -> IO ()) -- ^ action to run
           -> IO ()
withScreen w h act = do
    withInit [InitEverything] $ do
        s <- setVideoMode w h 32 [SWSurface]
        act s

--------------------------------
-- Game/Engine interface
--------------------------------

-- FIXME Also encode the EngineState in the state.
-- | Psych!  It's a state monad!
newtype Game s a = Game { runGame :: s -> (a, s) }

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
getGameState = Game (\s -> (s, s))

-- | Overwrite the current game state.
setGameState :: s -> Game s ()
setGameState s = Game (\_ -> ((), s))

-- | Pass the current game state through the given function.
modifyGameState :: (s -> s) -> Game s ()
modifyGameState f = Game (\s -> ((), f s))

data GameEvent = Tick | InputEvent Event

play :: forall w.
        (Int, Int)              -- ^ width, height of the game window
     -> Int                     -- ^ Logical ticks per second
     -> w                       -- ^ Game state
     -> (w -> Picture)          -- ^ How to draw a particular state
     -> (Event -> Game w ())    -- ^ How to update a state after an 'Event'
     -> (Double -> Game w ())   -- ^ How to update the state after a tick (the elapsed
                                -- time in seconds since the last tick is included)
     -> IO ()
play (screenW, screenH) tps wInit drawGame onEvent onTick = do
    withScreen screenW screenH $ \screen -> do
        putStrLn "Screen initialised"

        -- This channel is used to collect events from multiple sources for the game.
        eventCh <- newTChanIO

        -- Set up the first tick.
        tick eventCh

        -- Forward SDL event.
        forwardEvents eventCh

        playLoop screen eventCh wInit
  where
    playLoop :: Surface -> TChan GameEvent -> w -> IO ()
    playLoop screen eventCh w = do
        -- Block for next event.
        event <- atomically (readTChan eventCh)

        -- Notify game of event.
        let ((), w') = case event of
                -- FIXME Pass in the real time to onTick.
                Tick          -> runGame (onTick undefined) w
                InputEvent ev -> runGame (onEvent ev) w

        -- Draw the current state.
        draw screen idmtx $
            FilledRectangle 0 0 (fromIntegral screenW) (fromIntegral screenH) (Color 0 0 0 255)
        draw screen idmtx (drawGame w')
        flip screen

        -- Set up the next tick.
        case event of
            Tick -> tick eventCh
            _    -> return ()

        playLoop screen eventCh w'

    -- FIXME Actually tick at tps (not at slightly less).
    tick :: TChan GameEvent -> IO ()
    tick eventCh = void $ forkIO $ do
        threadDelay (1000000 `div` tps)
        atomically (writeTChan eventCh Tick)

    -- FIXME Have a way of stopping these threads.
    forwardEvents :: TChan GameEvent -> IO ()
    forwardEvents eventCh = void $ forkIO $ forever $ do
        event <- waitEvent
        atomically (writeTChan eventCh (InputEvent event))

--------------------------------
-- Runner
--------------------------------

main :: IO ()
main = do
    play (640, 480) 10 (0 :: Int) drawRects handleEvent handleTick
  where
    drawRects i =
        mconcat [ Translate (10 * fromIntegral (i `mod` 40)) 50 $
                  FilledRectangle 0 0 10 10 (Color 255 0 0 255)
                , Translate 30 (10 * fromIntegral (i `mod` 30)) $
                  FilledRectangle 0 0 10 10 (Color 0 255 0 255)
                ]

    handleEvent (KeyUp (Keysym {symKey = SDLK_ESCAPE})) =
        fail "escape!"
    handleEvent _ = do
        return ()

    handleTick _ = do
        modifyGameState (+1)
