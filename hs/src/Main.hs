{-# LANGUAGE NoMonomorphismRestriction #-}
import Graphics.Gloss as G
import Graphics.Gloss.Interface.Game as G

import Types
import Level

te :: Num a => a
te = fromIntegral tileEdge

dummyObject :: Pos -> Static
dummyObject pos = Static pic pos (3,1) path
  where
    path = [(0,0),(te*3,0),(te*3,-te),(0,-te)]
    pic  = G.color G.red $ G.Polygon path

transparencyTest :: IO Static
transparencyTest = do
    bmp <- fmap (G.translate 320 200) $ G.loadBMP "./resources/blah.bmp"
    let path = [(0,0),(640,0),(640,-400),(0,-400)]
    return (Static bmp (0,0) (64, 40) path)

displayMode :: G.Display
displayMode = G.InWindow "SuperMax" (1024, 768) (10, 10)

eventHandler :: G.Event -> Level -> Level
eventHandler (EventKey (SpecialKey k) Down _ _)
             l@(Level {levelCenter = (x, y)})
    = case k of
        KeyLeft  -> mov (-10, 0)
        KeyRight -> mov (10, 0)
        KeyUp    -> mov (0, 10)
        KeyDown  -> mov (0, -10)
        _        -> l
  where
    mov (x', y') = l {levelCenter = (x + x', y + y')}
eventHandler _ l = l


main :: IO ()
main = do
    tran <- transparencyTest
    let l = buildLevel [ LevelObject (dummyObject (0,0))
                       , LevelObject (dummyObject (3,1))
                       , LevelObject tran
                       ]
    G.play displayMode G.black 60 l drawLevel eventHandler step
