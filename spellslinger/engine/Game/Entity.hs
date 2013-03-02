{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Game.Entity (
        Entity(..), EntityId(..), Position(..), Behaviour(..)
    ) where

import Data.Set ( Set )
import Game.Engine ( Game, Picture )

newtype EntityId = EntityId Int
                 deriving ( Eq, Ord, Show )

newtype Position = Position (Int, Int)
                 deriving ( Eq, Ord, Show )

class Entity a where
    data EntityParameters a :: *

    init :: EntityParameters a -> Game w a
    eid :: a -> EntityId
    occupiedPositions :: a -> Set Position
    draw :: a -> Picture
    tickVisual :: a -> a

class (Entity a) => Behaviour w a where
    behave :: a -> Game w a
