{-# LANGUAGE NoMonomorphismRestriction #-}
import qualified Graphics.Gloss as G

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

main :: IO ()
main = do
    tran <- transparencyTest
    G.display (G.InWindow "SuperMax" (1024, 768) (10, 10)) G.black $ drawLevel $
        buildLevel [ LevelObject (dummyObject (0,0))
                   , LevelObject (dummyObject (3,1))
                   , LevelObject tran
                   ]