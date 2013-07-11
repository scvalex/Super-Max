{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Entities.RoomExit (
        RoomExit, EntityParameters(..), contains
    ) where

import Common ( intRectangle, fromAreaCoordinates, smallText )
import Control.Applicative ( (<$>) )
import Data.Monoid ( mconcat )
import qualified Data.Set as S
import SuperMax ( Picture(..), Colour(..), TextAlignment(..)
                , mkUid )
import SuperMax.Entity ( Entity(..), EntityId(..), Position(..) )

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

    occupiedPositions _ =
        S.empty -- You can walk over the room exit.

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

contains :: RoomExit -> Position -> Bool
contains (RE { getREPosition = Position (xe, ye) }) pos =
    let exitPoss = S.fromList [ Position (xe, ye), Position (xe + 1, ye)
                              , Position (xe, ye + 1), Position (xe + 1, ye + 1)
                              ] in
    pos `S.member` exitPoss
