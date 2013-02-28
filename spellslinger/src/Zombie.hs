{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Zombie (
        Zombie
    ) where

import Common ( intRectangle )
import Game.Engine ( Picture(..), Color(..) )
import Game.Entity ( Entity(..) )
import Types ( EntityId )

data Zombie = Zombie
    { getZombieId       :: EntityId
    , getZombiePosition :: (Int, Int)
    } deriving ( Eq, Show )

instance Entity w Zombie where
    data EntityParameters Zombie = ZombieParameters

    init ZombieParameters = do
        return (Zombie undefined undefined)

    entityId (Zombie { getZombieId = zid }) = zid

    draw _ (Zombie { getZombiePosition = (xz, yz) }) =
        Color (RGBA 255 0 0 255) $
        intRectangle xz yz 1 1

    tickVisual = id

    tick _ = do
        return ()
