-- | Dumping ground for common functions
module Common (
        -- * Drawing
        intRectangle
    ) where

import Game.Engine ( Picture(..) )

-- | Draw a polygon with 'Int' coordinates.
intRectangle :: Int -> Int -> Int -> Int -> Picture
intRectangle x1 y1 w h = FilledRectangle (fromIntegral x1)
                                         (fromIntegral y1)
                                         (fromIntegral w)
                                         (fromIntegral h)
