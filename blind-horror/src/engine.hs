{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ( flip )

import Control.Applicative ( Applicative(..) )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM ( atomically
                              , TChan, newTChanIO, readTChan, writeTChan )
import Control.Exception ( assert )
import Control.Monad ( when, void )
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
-- Event-loop
--------------------------------

handleEvent :: Event -> Bool
handleEvent (KeyUp (Keysym {symKey = SDLK_ESCAPE})) =
    False
handleEvent _ = do
    True

eventLoop :: IO ()
eventLoop = do
    event <- waitEvent
    when (handleEvent event) $ do
        eventLoop

--------------------------------
-- Game/Engine interface
--------------------------------

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

data GameEvent = Tick | SdlEvent Event

play :: forall w.
        (Int, Int)              -- ^ width, height of the game window
     -> Int                     -- ^ Logical ticks per second
     -> w                       -- ^ Game state
     -> (w -> Picture)          -- ^ How to draw a particular state
     -> (Event -> Game w ())    -- ^ How to update a state after an 'Event'
     -> (Double -> Game w ())   -- ^ How to update the state after a tick (the elapsed
                                -- time in seconds since the last tick is included)
     -> IO ()
play (sw, sh) tps wInit drawGame onEvent onTick = do
    withScreen sw sh $ \screen -> do
        putStrLn "Screen initialised"
        eventCh <- newTChanIO
        tick eventCh
        playLoop screen eventCh wInit
  where
    playLoop :: Surface -> TChan GameEvent -> w -> IO ()
    playLoop screen eventCh w = do
        event <- atomically (readTChan eventCh)
        let ((), w') = case event of
                Tick        -> runGame (onTick undefined) w
                SdlEvent ev -> runGame (onEvent ev) w
        draw screen idmtx (drawGame w')
        flip screen
        tick eventCh
        playLoop screen eventCh w'

    tick :: TChan GameEvent -> IO ()
    tick eventCh = void $ forkIO $ do
        threadDelay (1000000 `div` tps)
        atomically (writeTChan eventCh Tick)

--------------------------------
-- Runner
--------------------------------

main :: IO ()
main = do
    withScreen 640 480 $ \screen -> do
        putStrLn "Ok"
        draw screen idmtx $
            mconcat [ Translate 100 50 $
                      Scale 2 1.5 $
                      FilledRectangle 1 1 100 100 (Color 255 0 0 255)
                    , Translate 400 50 $
                      FilledRectangle 0 0 100 100 (Color 0 255 0 255)
                    ]
        flip screen
        eventLoop
