{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

import Graphics.UI.SDL.Image (load)
import Graphics.UI.SDL
import Control.Exception
import Prelude hiding (flip, init, span, max)

import Types
import Level
import Game
import Objects
import Exception
import Blitting
import Loop
import Player
import Common

te :: Num a => a
te = 30

squarePath :: Num a => [(a, a)]
squarePath = [(0,0),(te,0),(te,-te),(0,-te)]

tile :: Surface -> Point -> Animated
tile s (x, y) = Animated (staticAni s) (staticMov (x*te, y*te)) (te, te) squarePath

movingTile :: Surface -> Animated
movingTile s =
    Animated (staticAni s) (dynamicMov fmov ((te+span, 1), (te, 4*te)))
             (te, te) squarePath
  where
    span = 4 * te

    vel  :: Float
    vel  = 0.1

    fmov :: (Int, Int) -> Time -> Point -> ((Int, Int), Point)
    fmov (max, sign) delta (x, y) =
        let new = round $ fi x + fi delta * fi sign * vel
            ex  | sign == 1  = new >= max
                | otherwise = new <= max
        in if ex then
               ((max - span * sign, sign * (-1)), (max, y))
           else
               ((max, sign), (new, y))

player :: Surface -> Player
player s = Player { playerAni  = staticAni s
                  , playerPos  = (te, te)
                  , playerDim  = (te, te)
                  , playerBBox = squarePath
                  , playerDir  = Nothing
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
                               , LevelObject (movingTile s)
                               ]
                g = Game { gamePlayer = player p
                         , gameActDir = []
                         , gameLevel  = l
                         }
                draw g' = clearScreen screen >> blitGame g' screen >> flip screen
            loop 60 g draw step
