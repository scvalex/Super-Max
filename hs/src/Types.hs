module Types where

import Data.Word (Word32)
import Graphics.UI.SDL (Surface, Rect(..))

class TimeStep ts where
    step :: Word32 -> ts -> ts

class HasSurface a where
    surface :: a -> Surface

-- | A point in space
type Point = (Int, Int)

-- | A polygon path
type Path = [Point]

translate :: Point -> Point -> Point
translate (x, y) (x', y') = (x' + x, y' + y)

-- | The amount of tiles that the object occupies
type Dimension = (Int, Int)

class (TimeStep o, HasSurface o) => Object o where
    -- | The 'Rect' in which the object resides
    rect :: o -> Rect
    -- | The bounding box of the object.
    bbox :: o -> Path
