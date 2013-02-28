{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Game.Entity (
        Entity(..)
    ) where

import Game.Engine ( Game, Picture )
import Types ( EntityId )

class Entity w a where
    data EntityParameters a :: *

    init :: EntityParameters a -> Game w a
    entityId :: a -> EntityId
    draw :: w -> a -> Picture
    tickVisual :: a -> a
    tick :: a -> Game w ()
