{-# LANGUAGE NoMonomorphismRestriction #-}
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Game as G

import Types
import Level
import Game

te :: Num a => a
te = fromIntegral tileEdge

squarePath :: Num a => [(a, a)]
squarePath = [(0,0),(te,0),(te,-te),(0,-te)]

square :: G.Color -> G.Picture
square col = G.color col $ G.Polygon squarePath

dummyObject :: Point -> Static
dummyObject (x, y) = Static (square G.red) (x*tileEdge, y*tileEdge) (30,30) squarePath

-- transparencyTest :: IO Static
-- transparencyTest = do
--     bmp <- fmap (translate 320 200) $ loadBMP "./resources/blah.bmp"
--     let path = [(0,0),(640,0),(640,-400),(0,-400)]
--     return (Static bmp (0,0) (64, 40) path)

displayMode :: G.Display
displayMode = G.InWindow "SuperMax" (1024, 768) (10, 10)

eventHandler :: G.Event -> Game -> Game
eventHandler (G.EventKey (G.SpecialKey k) G.Down _ _)
             g@(Game {gamePlayer = p}) =
    case k of
        G.KeyLeft  -> mov (-10, 0)
        G.KeyRight -> mov (10, 0)
        G.KeyUp    -> mov (0, 10)
        G.KeyDown  -> mov (0, -10)
        _        -> g
  where
    mov m       = g {gamePlayer = movP m}
    movP (x, y) = let (px, py) = animatedPos p
                  in  p {animatedPos = (px + x, py + y)}
eventHandler _ g = g

player :: Animated
player = Animated { animatedAni  = staticAni (square G.blue)
                  , animatedPos  = (60, 60)
                  , animatedDim  = (30, 30)
                  , animatedBBox = squarePath
                  }

main :: IO ()
main = do
    let l = buildLevel [ LevelObject (dummyObject (-1,1))
                       , LevelObject (dummyObject (0,0))
                       , LevelObject (dummyObject (1,0))
                       , LevelObject (dummyObject (2,0))
                       , LevelObject (dummyObject (3,0))
                       , LevelObject (dummyObject (4,1))
                       , LevelObject (dummyObject (5,1))
                       , LevelObject (dummyObject (6,1))
                       , LevelObject (dummyObject (7,1))
                       ]
        g = Game { gamePlayer = player
                 , gameActDir = []
                 , gameLevel  = l
                 }
    G.play displayMode G.black 60 g draw eventHandler step
