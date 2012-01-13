module Types
    ( TimeStep (..)
    , tileEdge
    , Object (..)
    , Static (..)
    , Path
    , Point
    ) where

import qualified Graphics.Gloss as G

class TimeStep ts where
    step :: Float -> ts -> ts

tileEdge :: Int
tileEdge = 30

-- | A point in space
type Point = (Int, Int)

-- | A polygon path
type Path = [Point]

-- | A grid position
type Pos = (Int, Int)

-- | The amount of tiles that the object occupies
type Dimension = (Int, Int)

class TimeStep o => Object o where
    -- | Produces an image with (0,0) aligned with the top-left corner of the
    --   image. Its point dimensions must be a multiple of 'tileEdge'.
    draw       :: o -> G.Picture
    -- | The position of the object on the grid (referring to the tiles
    -- coordinates).
    position   :: o -> Pos
    -- | How many tiles the object occupies.
    dimensions :: o -> Dimension
    -- | The internal bounding box of the object.
    bbox       :: o -> Path

-- | A static object which does not do anything in time.
data Static = Static
    { staticPic  :: G.Picture
    , staticPos  :: Pos
    , staticDim  :: (Int, Int)
    , staticBBox :: Path
    } deriving (Show, Eq)

instance TimeStep Static where
    step _ s = s

instance Object Static where
    draw (Static {staticPic = pic})       = pic
    position (Static {staticPos = pos})   = pos
    dimensions (Static {staticDim = dim}) = dim
    bbox (Static {staticBBox = box})      = box
