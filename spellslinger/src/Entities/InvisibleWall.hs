{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Entities.InvisibleWall (
        InvisibleWall, EntityParameters(..)
    ) where

import Control.Applicative ( (<$>) )
import Data.Set ( Set )
import Data.Vect.Float ( idmtx )
import qualified Data.Set as S
import SuperMax ( Drawable(..), mkUid
                , Entity(..), EntityId(..), Position(..) )

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

    tickVisual = id

instance Drawable InvisibleWall where
    drawableVertices _    = []
