module Main where

import Control.Concurrent ( threadDelay )
import Graphics.UI.SDL ( InitFlag(..), withInit
                       , Surface, SurfaceFlag(..), setVideoMode )

main :: IO ()
main = do
    withScreen 640 480 $ \_ -> do
        putStrLn "Ok"
        threadDelay 1000000

withScreen :: Int                -- ^ width
           -> Int                -- ^ height
           -> (Surface -> IO ()) -- ^ action to run
           -> IO ()
withScreen w h act = do
    withInit [InitEverything] $ do
        s <- setVideoMode w h 32 [SWSurface]
        act s
