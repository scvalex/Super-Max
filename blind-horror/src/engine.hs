{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ( flip )

import Control.Concurrent ( threadDelay )
import Control.Exception ( assert )
import Data.Word ( Word8 )
import Graphics.UI.SDL ( InitFlag(..), withInit
                       , Surface, SurfaceFlag(..), setVideoMode, flip
                       , Rect(..), mapRGBA
                       , surfaceGetPixelFormat, fillRect )
import Text.Printf ( printf )

data Color = Color { colorRed   :: Word8
                   , colorGreen :: Word8
                   , colorBlue  :: Word8
                   , colorAlpha :: Word8
                   } deriving (Eq)

instance Show Color where
    show c = printf "#%2x%2x%2x%2x" (colorRed c) (colorGreen c) (colorBlue c) (colorAlpha c)

data Picture = FilledRectangle Double Double Double Double Color
             deriving ( Eq, Show )

draw :: Surface -> Picture -> IO ()
draw surface (FilledRectangle x y w h c) = do
    let pf = surfaceGetPixelFormat surface
    p <- mapRGBA pf (colorRed c) (colorGreen c) (colorBlue c) (colorAlpha c)
    let r = Rect { rectX = floor x, rectY = floor y
                 , rectW = floor w, rectH = floor h }
    ok <- fillRect surface (Just r) p
    assert ok (return ())

main :: IO ()
main = do
    withScreen 640 480 $ \screen -> do
        putStrLn "Ok"
        draw screen (FilledRectangle 1.0 1.0 100.0 100.0 (Color 255 0 0 255))
        flip screen
        threadDelay 1000000

withScreen :: Int                -- ^ width
           -> Int                -- ^ height
           -> (Surface -> IO ()) -- ^ action to run
           -> IO ()
withScreen w h act = do
    withInit [InitEverything] $ do
        s <- setVideoMode w h 32 [SWSurface]
        act s
