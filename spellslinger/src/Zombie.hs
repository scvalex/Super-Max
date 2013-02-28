{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Zombie (
        Zombie
    ) where

import Common ( intRectangle, fromAreaCoordinates )
import Control.Applicative ( (<$>) )
import Game.Engine ( Picture(..), Color(..), mkUid, randomR )
import Game.Entity ( Entity(..) )
import Types ( EntityId(..) )

data Zombie = Zombie
    { getZombieId         :: EntityId
    , getZombiePosition   :: (Int, Int)
    , getZombieAreaBounds :: (Int, Int, Int, Int)
    } deriving ( Eq, Show )

instance Entity w Zombie where
    data EntityParameters Zombie = RandomZombie
        { getAreaBounds :: (Int, Int, Int, Int)
        }

    init zp@(RandomZombie { getAreaBounds = (x1, y1, x2, y2) }) = do
        zid <- EntityId <$> mkUid
        xz <- randomR (x1, x2)
        yz <- randomR (y1, y2)
        return (Zombie { getZombieId         = zid
                       , getZombiePosition   = (xz, yz)
                       , getZombieAreaBounds = getAreaBounds zp
                       })

    entityId (Zombie { getZombieId = zid }) = zid

    draw (Zombie { getZombiePosition = (xz, yz), getZombieAreaBounds = bounds }) =
        fromAreaCoordinates bounds $
        Color (RGBA 255 0 0 255) $
        intRectangle xz yz 1 1

    tickVisual = id
