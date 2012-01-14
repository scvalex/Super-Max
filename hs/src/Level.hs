{-# LANGUAGE ExistentialQuantification, StandaloneDeriving, Rank2Types #-}
module Level where

import Graphics.Gloss (pictures)

import Types

data LevelObject = forall o. (Object o, Show o) => LevelObject o

deriving instance Show LevelObject

type Tag = Int

newtype Level = Level {levelObjects :: [LevelObject]}
    deriving (Show)

instance TimeStep Level where
    step f l = l {levelObjects = map (\(LevelObject o) -> LevelObject (step f o))
                                     (levelObjects l)}

tileEdge :: Int
tileEdge = 30

buildLevel :: [LevelObject] -> Level
buildLevel = Level

instance Drawable Level where
    draw (Level os) =
        pictures . map (\(LevelObject o) -> draw o) $ os
