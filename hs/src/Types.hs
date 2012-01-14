module Types
    ( TimeStep (..)
    , Drawable (..)
    , Object (..)
    , Static (..)
    , Path
    , Point
    , Dimension
    , Ani (..)
    , applyAni
    , animationPic
    , staticAni
    , Animated (..)
    ) where

import Graphics.Gloss (Picture, translate)

import Common

class TimeStep ts where
    step :: Float -> ts -> ts

class Drawable a where
    draw :: a -> Picture

-- | A point in space
type Point = (Int, Int)

-- | A polygon path
type Path = [Point]


-- | The amount of tiles that the object occupies
type Dimension = (Int, Int)

-- | Produces an image with (0,0) aligned with the top-left corner of the
--   image. Its point dimensions must be a multiple of 'tileEdge'.
class (TimeStep o, Drawable o) => Object o where
    -- | The position of the object on the grid (referring to the tiles
    -- coordinates).
    position   :: o -> Point
    -- | How many tiles the object occupies.
    dimensions :: o -> Dimension
    -- | The internal bounding box of the object.
    bbox       :: o -> Path

-- | A static object which does not do anything in time.
data Static = Static
    { staticPic  :: Picture
    , staticPos  :: Point
    , staticDim  :: Dimension
    , staticBBox :: Path
    } deriving (Show, Eq)

instance TimeStep Static where
    step _ s = s

instance Drawable Static where
    draw (Static {staticPic = pic, staticPos = (x, y)}) =
        translate (fi x) (fi y) pic

instance Object Static where
    position (Static {staticPos = pos})   = pos
    dimensions (Static {staticDim = dim}) = dim
    bbox (Static {staticBBox = box})      = box

data Ani = Ani Picture (Float -> Ani)

applyAni :: Ani -> Float -> Ani
applyAni (Ani _ f) fl = f fl

animationPic :: Ani -> Picture
animationPic (Ani pic _) = pic

staticAni :: Picture -> Ani
staticAni p = Ani p (\_ -> staticAni p)

data Animated = Animated
    { animatedAni  :: Ani
    , animatedPos  :: Point
    , animatedDim  :: Dimension
    , animatedBBox :: Path
    }

instance TimeStep Animated where
    step f p@(Animated {animatedAni = ani}) =
        p {animatedAni = applyAni ani f}

instance Drawable Animated where
    draw (Animated {animatedAni = ani, animatedPos = (x, y)}) =
        translate (fi x) (fi y) $ animationPic ani

instance Object Animated where
    position (Animated {animatedPos = pos})   = pos
    dimensions (Animated {animatedDim = dim}) = dim
    bbox (Animated {animatedBBox = box})      = box
