{-# LANGUAGE ExistentialQuantification, StandaloneDeriving, Rank2Types #-}
module Level where

import Types

data LevelObject = forall o. (Object o, Show o) => LevelObject o

deriving instance Show LevelObject

type Tag = Int

newtype Level = Level {levelObjects :: [LevelObject]}
    deriving (Show)

instance TimeStep Level where
    step f l = l {levelObjects = map (\(LevelObject o) -> LevelObject (step f o))
                                     (levelObjects l)}

buildLevel :: [LevelObject] -> Level
buildLevel = Level
