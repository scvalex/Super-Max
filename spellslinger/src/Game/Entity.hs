{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, ExistentialQuantification #-}

module Game.Entity (
        Entity(..), Behaviour(..), SomeEntity(..)
    ) where

import Game.Engine ( Game, Picture )
import Types ( Position, EntityId )

data SomeEntity w = forall a. Entity w a
                  => SomeEntity a

class Entity w a where
    data EntityParameters a :: *

    init :: EntityParameters a -> Game w a
    entityId :: a -> EntityId
    entityPosition :: a -> Position
    draw :: a -> Picture
    tickVisual :: a -> a

class (Entity w a) => Behaviour w a b where
    behave :: a -> b -> Game w a
