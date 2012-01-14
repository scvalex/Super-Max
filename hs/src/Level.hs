{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
module Level where

import Data.Array.IArray
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Graphics.Gloss as G

import Types

data LevelObject = forall o. (Object o, Show o) => LevelObject o

deriving instance Show LevelObject

type Tag = Int

data Level = Level
     { levelGrid    :: Array (Int, Int) (Maybe Tag)
     , levelObjects :: IntMap LevelObject
     , levelCenter  :: Point
     } deriving (Show)

instance TimeStep Level where
    step f l = l {levelObjects = IM.map (\(LevelObject o) -> LevelObject (step f o))
                                        (levelObjects l)}

buildLevel :: [LevelObject] -> Level
buildLevel [] = Level { levelGrid    = array ((0,0), (0,0)) []
                      , levelObjects = IM.empty
                      , levelCenter  = (0, 0)
                      }
buildLevel os = Level { levelGrid    = grid
                      , levelObjects = IM.fromList osix
                      , levelCenter  = (0, 0)
                      }
  where
    osix = zip [1..] os
    poss = map (\(LevelObject o) -> position o) os
    bl   = minimum poss
    tr   = maximum poss
    base = array (bl, tr) [ ((x, y), Nothing)
                          | x <- [fst bl..fst tr]
                          , y <- [snd bl..snd tr]
                          ]
    grid = base // map (\(t, LevelObject o) -> (position o, Just t)) osix

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

drawLevel :: Level -> G.Picture
drawLevel (Level {levelObjects = os, levelCenter = (cx, cy)}) =
    G.translate (fi cx) (fi cy) . G.pictures . map trans . map snd .
    IM.toList $ os
  where
    trans (LevelObject o) =
        let (x, y) = position o
        in G.translate (fi (tileEdge * x)) (fi (tileEdge * y)) $ draw o
