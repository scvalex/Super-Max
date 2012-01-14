module Types
    ( Time
    , Step(..)
    , HasSurface(..)
    , Point
    , Path
    , translate
    , Dimension
    , Object(..)
    ) where

import Data.Word (Word32)
import Graphics.UI.SDL (Surface, Rect(..), Event)

type Time = Word32

class Step a where
    step :: Time -> Event -> a -> IO a

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

class (Step o, HasSurface o) => Object o where
    -- | The 'Rect' in which the object resides
    rect   :: o -> Rect
    -- | The bounding box of the object.
    bbox   :: o -> Path
