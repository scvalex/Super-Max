{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Zombie (
        Zombie, EntityParameters(..),
        setPosition
    ) where

import Common ( intRectangle, fromAreaCoordinates )
import Control.Applicative ( (<$>) )
import Game.Engine ( Picture(..), Colour(..), mkUid, randomR )
import Game.Entity ( Entity(..) )
import Types ( EntityId(..), Position(..) )

data Zombie = Zombie
    { getZombieId         :: EntityId
    , getZombiePosition   :: Position
    , getZombieAreaBounds :: (Int, Int, Int, Int)
    , getZombieVisualTint :: Double
    } deriving ( Eq, Show )

instance Entity Zombie where
    data EntityParameters Zombie = RandomZombie
        { getAreaBounds :: (Int, Int, Int, Int)
        }

    init zp@(RandomZombie { getAreaBounds = (x1, y1, x2, y2) }) = do
        zid <- EntityId <$> mkUid
        xz <- randomR (x1, x2)
        yz <- randomR (y1, y2)
        return (Zombie { getZombieId         = zid
                       , getZombiePosition   = Position (xz, yz)
                       , getZombieAreaBounds = getAreaBounds zp
                       , getZombieVisualTint = pi
                       })

    eid (Zombie { getZombieId = zid }) = zid

    position (Zombie { getZombiePosition = pos }) = pos

    draw (Zombie { getZombiePosition   = Position (xz, yz)
                 , getZombieAreaBounds = bounds
                 , getZombieVisualTint = tint }) =
        fromAreaCoordinates bounds $
        Colour (RGBA 0 (180 + floor (75.0 * sin tint)) 0 255) $
        intRectangle xz yz 1 1

    tickVisual z@(Zombie { getZombieVisualTint = tint }) =
        z { getZombieVisualTint = tint + pi / 16 }

setPosition :: Zombie -> Position -> Zombie
setPosition z pos = z { getZombiePosition = pos }
