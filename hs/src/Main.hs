import qualified Graphics.Gloss as G

import Types
import Level

dummyObject :: Static
dummyObject = Static pic (0,0) (3,1) path
  where
    te   :: Num a => a
    te   = fromIntegral tileEdge
    path :: Num a => [(a, a)]
    path = [(0,0),(te*3,0),(te*3,-te),(0,-te)]
    pic  = G.color G.red $ G.Polygon path

main :: IO ()
main =
    G.display (G.InWindow "SuperMax" (1024, 768) (10, 10)) G.black $
    drawLevel (buildLevel [LevelObject dummyObject])