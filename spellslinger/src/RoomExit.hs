{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module RoomExit (
        RoomExit, EntityParameters(..)
    ) where

import Common ( intRectangle, fromAreaCoordinates, smallText )
import Control.Applicative ( (<$>) )
import Data.Monoid ( mconcat )
import Game.Engine ( Picture(..), Colour(..), TextAlignment(..)
                   , mkUid )
import Game.Entity ( Entity(..) )
import Types ( EntityId(..), Position(..) )

data RoomExit = RE
    { getREId         :: EntityId
    , getREPosition   :: Position
    , getREAreaBounds :: (Int, Int, Int, Int)
    } deriving ( Eq, Show )

instance Entity RoomExit where
    data EntityParameters RoomExit = StaticExit
        { getAreaBounds       :: (Int, Int, Int, Int)
        , getAreaExitPosition :: Position
        }

    init (StaticExit { getAreaBounds       = areaBounds
                     , getAreaExitPosition = exitPos }) = do
        reid <- EntityId <$> mkUid
        return (RE { getREId         = reid
                   , getREPosition   = exitPos
                   , getREAreaBounds = areaBounds
                   })

    eid (RE { getREId = reid }) = reid

    position (RE { getREPosition = pos }) = pos

    draw (RE { getREPosition   = Position (xe, ye)
             , getREAreaBounds = bounds }) =
        fromAreaCoordinates bounds $
        mconcat [ Colour (RGBA 255 215 0 255) $
                  intRectangle xe ye 2 1
                , Translate (fromIntegral (xe + 1)) (fromIntegral (ye + 1)) $
                  smallText CenterAligned "Exit"
                ]

    tickVisual = id
