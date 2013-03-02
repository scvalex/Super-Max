{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module RoomExit (
        RoomExit, EntityParameters(..)
    ) where

import Common ( intRectangle, fromAreaCoordinates, smallText )
import Control.Applicative ( (<$>) )
import Data.Monoid ( mconcat )
import Game.Engine ( Picture(..), Colour(..), TextAlignment(..)
                   , mkUid )
import Game.Entity ( Entity(..), EntityId(..), Position(..) )
import qualified Data.Set as S

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

    positions (RE { getREPosition = pos@(Position (xe, ye)) }) =
        S.fromList [ pos, Position (xe + 1, ye)
                   , Position (xe, ye + 1), Position (xe + 1, ye + 1)
                   ]

    draw (RE { getREPosition   = Position (xe, ye)
             , getREAreaBounds = bounds }) =
        fromAreaCoordinates bounds $
        Translate (fromIntegral xe) (fromIntegral ye) $
        mconcat [ Colour (RGBA 255 215 0 255) $
                  intRectangle 0 0 2 2
                , Colour (RGBA 0 0 0 255) $
                  -- FIXME Why are the proportions fsckup up here?
                  FilledRectangle 0.2 0.2 1.7 1.5
                , Translate 1 2 $
                  smallText CenterAligned "Exit"
                ]

    tickVisual = id
