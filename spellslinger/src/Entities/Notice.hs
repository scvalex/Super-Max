{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Entities.Notice (
        Notice, EntityParameters(..)
    ) where

import Common ( intRectangle, fromAreaCoordinates, smallText )
import Control.Applicative ( (<$>) )
import Data.Monoid ( mconcat )
import Game.Engine ( Picture(..), Colour(..), TextAlignment(..)
                   , mkUid )
import Game.Entity ( Entity(..), EntityId(..), Position(..) )
import qualified Data.Set as S

data Notice = Notice
    { getNoticeId         :: EntityId
    , getNoticePosition   :: Position
    , getNoticeText       :: String
    , getNoticeAreaBounds :: (Int, Int, Int, Int)
    } deriving ( Eq, Show )

instance Entity Notice where
    data EntityParameters Notice = StaticNotice
        { getAreaBounds     :: (Int, Int, Int, Int)
        , getStaticPosition :: Position
        , getStaticText     :: String
        }

    init (StaticNotice { getAreaBounds     = areaBounds
                       , getStaticPosition = pos
                       , getStaticText     = text }) = do
        nid <- EntityId <$> mkUid
        return (Notice { getNoticeId         = nid
                       , getNoticePosition   = pos
                       , getNoticeText       = text
                       , getNoticeAreaBounds = areaBounds
                       })

    eid (Notice { getNoticeId = nid }) = nid

    positions (Notice { getNoticePosition = pos }) = S.singleton pos

    draw (Notice { getNoticePosition   = Position (xn, yn)
                 , getNoticeAreaBounds = bounds
                 , getNoticeText       = text }) =
        fromAreaCoordinates bounds $
        Translate (fromIntegral xn) (fromIntegral yn) $
        mconcat [ Colour (RGBA 160 160 255 255) $
                  intRectangle 0 0 1 1
                , Translate 0.5 1 $
                  smallText CenterAligned text
                ]

    tickVisual = id
