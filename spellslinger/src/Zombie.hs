{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Zombie (
        Zombie
    ) where

import Common ( intRectangle )
import Control.Applicative ( (<$>) )
import Game.Engine ( Picture(..), Color(..), mkUid )
import Game.Entity ( Entity(..) )
import Types ( EntityId(..) )

data Zombie = Zombie
    { getZombieId       :: EntityId
    , getZombiePosition :: (Int, Int)
    } deriving ( Eq, Show )

instance Entity w Zombie where
    data EntityParameters Zombie = ZombieParameters

    init ZombieParameters = do
        zid <- EntityId <$> mkUid
        return (Zombie { getZombieId       = zid
                       , getZombiePosition = undefined
                       })

    entityId (Zombie { getZombieId = zid }) = zid

    draw _ (Zombie { getZombiePosition = (xz, yz) }) =
        Color (RGBA 255 0 0 255) $
        intRectangle xz yz 1 1

    tickVisual = id

    tick _ = do
        return ()
