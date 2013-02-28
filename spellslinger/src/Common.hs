-- | Dumping ground for common functions
module Common (
        -- * Drawing
        intRectangle, fromAreaCoordinates
    ) where

import Game.Engine ( Picture(..) )

-- | Draw a polygon with 'Int' coordinates.
intRectangle :: Int -> Int -> Int -> Int -> Picture
intRectangle x1 y1 w h = FilledRectangle (fromIntegral x1)
                                         (fromIntegral y1)
                                         (fromIntegral w)
                                         (fromIntegral h)

-- Convert a picture in area coordinates to one in drawing coordinates.
fromAreaCoordinates :: (Int, Int, Int, Int) -> Picture -> Picture
fromAreaCoordinates (x1, y1, x2, y2) =
    Translate (fromIntegral x1) (fromIntegral y1) .
    Scale (1.0 / fromIntegral (x2 - x1)) (1.0 / fromIntegral (y2 - y1))
