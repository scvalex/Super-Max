{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Entities.InvisibleWall (
        InvisibleWall, EntityParameters(..)
    ) where

import Control.Applicative ( (<$>) )
import Data.Monoid ( mempty )
import Data.Set ( Set )
import Game.Engine ( mkUid )
import Game.Entity ( Entity(..), EntityId(..), Position(..) )
import qualified Data.Set as S

data InvisibleWall = InvisibleWall
    { getIWId           :: EntityId
    , getIWOccupiedPoss :: Set Position
    } deriving ( Eq, Show )

instance Entity InvisibleWall where
    data EntityParameters InvisibleWall = BorderWall
        { getAreaBounds :: (Int, Int, Int, Int)
        }

    init (BorderWall { getAreaBounds = (x1, y1, x2, y2) }) = do
        iwid <- EntityId <$> mkUid
        let occupiedPoss = S.unions [ S.fromList [ Position (x1 - 1, y) | y <- [y1..y2] ]
                                    , S.fromList [ Position (x2 + 1, y) | y <- [y1..y2] ]
                                    , S.fromList [ Position (x, y1 - 1) | x <- [x1..x2] ]
                                    , S.fromList [ Position (x, y2 + 1) | x <- [x1..x2] ]
                                    ]
        return (InvisibleWall { getIWId           = iwid
                              , getIWOccupiedPoss = occupiedPoss
                              })

    eid (InvisibleWall { getIWId = iwid }) = iwid

    occupiedPositions (InvisibleWall { getIWOccupiedPoss = occupiedPoss }) =
        occupiedPoss

    draw _ = mempty

    tickVisual = id
