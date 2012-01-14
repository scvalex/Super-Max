{-# LANGUAGE NoMonomorphismRestriction #-}

import Graphics.UI.SDL.Image (load)
import Graphics.UI.SDL
import Control.Exception
import Prelude hiding (flip, init)

import Types
import Level
import Game
import Objects
import Exception
import Blitting
import Loop

te :: Num a => a
te = 30

squarePath :: Num a => [(a, a)]
squarePath = [(0,0),(te,0),(te,-te),(0,-te)]

tile :: Surface -> Point -> Static
tile s (x, y) = Static s (x*te, y*te) (te, te) squarePath

player :: Surface -> Animated
player s = Animated { animatedAni  = staticAni s
                    , animatedPos  = (te, te)
                    , animatedDim  = (te, te)
                    , animatedBBox = squarePath
                    }

clearScreen :: Surface -> IO ()
clearScreen screen = fillRect screen Nothing (Pixel 0) >> return ()

main :: IO ()
main = do
    init [InitEverything]
    screenM <- trySetVideoMode 1024 768 32 [SWSurface]
    case screenM of
        Nothing     -> throwIO FailedSetVideoMode
        Just screen -> do
            s <- load "./resources/red.png"
            p <- load "./resources/player.png"
            let l = buildLevel [ LevelObject (tile s (0,1))
                               , LevelObject (tile s (1,2))
                               , LevelObject (tile s (2,2))
                               , LevelObject (tile s (3,2))
                               , LevelObject (tile s (4,2))
                               , LevelObject (tile s (5,1))
                               , LevelObject (tile s (6,1))
                               , LevelObject (tile s (7,1))
                               , LevelObject (tile s (8,1))
                               ]
                g = Game { gamePlayer = player p
                         , gameActDir = []
                         , gameLevel  = l
                         }
                draw g' = clearScreen screen >> blitGame g' screen >> flip screen
            loop 60 g draw updateGame
