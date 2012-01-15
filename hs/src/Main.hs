import Control.Exception
import Prelude hiding (flip, init, span, max)

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

import Animated
import Blitting
import Common
import Exception
import Game
import Loop

te :: Num a => a
te = 30

squareBox :: BBox
squareBox = [Rect 0 0 (te-1) (te-1)]

tile :: Surface -> Point -> Animated
tile s (x, y) =
    Animated (staticAni s) (staticMov (x*te, y*te)) (te-1, te-1) squareBox

movingTile :: Surface -> Animated
movingTile s =
    Animated (staticAni s) (dynamicMov fmov ((te+span, 1), (te, 4*te)))
             (te, te) squareBox
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
                  , playerDim  = (te-1, te-1)
                  , playerBBox = squareBox
                  , playerDir  = Nothing
                  }

clearScreen :: Surface -> IO ()
clearScreen screen = fillRect screen Nothing (Pixel 0) >> return ()

level :: Surface -> Level
level s = buildLevel [ LevelObject (tile s (0,1))
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
main :: IO ()
main = do
    init [InitEverything]
    screenM <- trySetVideoMode 1024 768 32 [SWSurface]
    case screenM of
        Nothing     -> throwIO FailedSetVideoMode
        Just screen -> do
            s <- load "./resources/red.png"
            p <- load "./resources/player.png"
            let l = level s
                g = Game { gamePlayer = player p
                         , gameActDir = []
                         , gameLevel  = l
                         }
                draw g' = clearScreen screen >> blitGame g' screen >> flip screen
            loop 60 g draw stepGame
