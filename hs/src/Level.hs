{-# LANGUAGE ExistentialQuantification, StandaloneDeriving, Rank2Types #-}
module Level
    ( LevelObject(..)
    , Level(..)
    , buildLevel
    ) where

import Control.Monad

import Types

data LevelObject = forall o. Object o => LevelObject o

newtype Level = Level {levelObjects :: [LevelObject]}

instance Step Level where
    step d ev l = do
        os <- forM (levelObjects l) $
              \(LevelObject o) -> fmap LevelObject (step d ev o)
        return $ l {levelObjects = os}

buildLevel :: [LevelObject] -> Level
buildLevel = Level
