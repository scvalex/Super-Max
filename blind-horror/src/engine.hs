{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ( flip )

import Control.Concurrent ( threadDelay )
import Control.Exception ( assert )
import Data.Monoid ( Monoid(..) )
import Data.Word ( Word8 )
import Data.Vect.Double ( Mat3(..), Matrix(..), LeftModule(..), Vec3(..)
                        , MultSemiGroup(..) )
import Graphics.UI.SDL ( InitFlag(..), withInit
                       , Surface, SurfaceFlag(..), setVideoMode, flip
                       , Rect(..), mapRGBA
                       , surfaceGetPixelFormat, fillRect )
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
        s <- setVideoMode w h 32 [HWSurface]
        act s

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
        threadDelay 1000000
