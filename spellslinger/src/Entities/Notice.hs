{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Entities.Notice (
        Notice, EntityParameters(..),
        getPosition, activated, deactivated
    ) where

import Common ( intRectangle, fromAreaCoordinates, smallText )
import Control.Applicative ( (<$>) )
import Data.Monoid ( Monoid(..) )
import qualified Data.Set as S
import SuperMax ( Game, Picture(..), Colour(..), TextAlignment(..)
                , mkUid, randomR
                , Entity(..), EntityId(..), Position(..) )

data Notice = Notice
    { getNoticeId         :: EntityId
    , getNoticePosition   :: Position
    , getNoticeActive     :: Bool
    , getNoticeText       :: String
    , getNoticeAreaBounds :: (Int, Int, Int, Int)
    } deriving ( Eq, Show )

instance Entity Notice where
    data EntityParameters Notice = StaticNotice { getAreaBounds     :: (Int, Int, Int, Int)
                                                , getStaticPosition :: Position
                                                , getStaticText     :: String
                                                }
                                 | RandomNotice { getAreaBounds     :: (Int, Int, Int, Int)
                                                , getStaticPosition :: Position
                                                , getPossibleTexts  :: [String]
                                                }

    init (RandomNotice { getAreaBounds     = areaBounds
                       , getStaticPosition = pos
                       , getPossibleTexts  = texts }) = do
        i <- randomR (0, length texts - 1)
        mkNotice areaBounds pos (texts !! i)
    init (StaticNotice { getAreaBounds     = areaBounds
                       , getStaticPosition = pos
                       , getStaticText     = text }) = do
        mkNotice areaBounds pos text

    eid (Notice { getNoticeId = nid }) = nid

    occupiedPositions (Notice { getNoticePosition = pos }) =
        S.singleton pos

    draw (Notice { getNoticePosition   = Position (xn, yn)
                 , getNoticeAreaBounds = bounds
                 , getNoticeActive     = active
                 , getNoticeText       = text }) =
        fromAreaCoordinates bounds $
        Translate (fromIntegral xn) (fromIntegral yn) $
        let colour = if active
                     then Colour (RGBA 220 220 255 255)
                     else Colour (RGBA 160 160 255 255) in
        let msg = if active
                  then Translate 0.5 1 $
                       smallText CenterAligned text
                  else mempty in
        -- FIXME Add some sort of palette, informally or explicitly.
        mconcat [ Colour (RGBA 50 50 160 255) $
                  intRectangle 0 0 1 1
                , colour $
                  FilledRectangle 0.2 0.1 0.7 0.7
                , msg
                ]

    tickVisual = id

mkNotice :: (Int, Int, Int, Int) -> Position -> String -> Game s Notice
mkNotice areaBounds pos text = do
    nid <- EntityId <$> mkUid
    return (Notice { getNoticeId         = nid
                   , getNoticePosition   = pos
                   , getNoticeActive     = False
                   , getNoticeText       = text
                   , getNoticeAreaBounds = areaBounds
                   })

getPosition :: Notice -> Position
getPosition (Notice { getNoticePosition = pos }) = pos

activated :: Notice -> Notice
activated n = n { getNoticeActive = True }

deactivated :: Notice -> Notice
deactivated n = n { getNoticeActive = False }
