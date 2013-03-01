{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Game.Entity (
        Entity(..), Behaviour(..)
    ) where

import Game.Engine ( Game, Picture )
import Types ( Position, EntityId )

class Entity a where
    data EntityParameters a :: *

    init :: EntityParameters a -> Game w a
    eid :: a -> EntityId
    position :: a -> Position
    draw :: a -> Picture
    tickVisual :: a -> a

class (Entity a) => Behaviour w a where
    behave :: a -> Game w a
