{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ( flip )

import Control.Concurrent ( threadDelay )
import Control.Exception ( assert )
import Data.Word ( Word8 )
import Data.Vect.Double ( Mat3(..), Matrix(..), LeftModule(..), Vec3(..)
                        , MultSemiGroup(..) )
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
             | Translate Double Double Picture
             deriving ( Eq, Show )

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
draw surface proj (Translate w h picture) = do
    let proj' = proj .*. (Mat3 (Vec3 1 0 w) (Vec3 0 1 h) (Vec3 0 0 1))
    draw surface proj' picture

main :: IO ()
main = do
    withScreen 640 480 $ \screen -> do
        putStrLn "Ok"
        draw screen idmtx (Translate 100 50 $
                           FilledRectangle 1 1 100 100 (Color 255 0 0 255))
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
