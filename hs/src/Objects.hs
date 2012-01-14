module Objects where

import Graphics.UI.SDL (Surface, Rect(..))
import Data.Word (Word32)

import Types

-- | A static object which does not do anything in time.
data Static = Static
    { staticSur  :: Surface
    , staticPos  :: Point
    , staticDim  :: Dimension
    , staticBBox :: Path
    } deriving (Show, Eq)

instance TimeStep Static where
    step _ s = s

instance HasSurface Static where
    surface (Static {staticSur = sur}) = sur

instance Object Static where
    rect (Static {staticPos = (x, y), staticDim = (w, h)}) = Rect x y w h
    bbox (Static {staticPos = pos, staticBBox = box})
        = map (translate pos) box

data Ani = Ani Surface (Word32 -> Ani)

applyAni :: Ani -> Word32 -> Ani
applyAni (Ani _ f) fl = f fl

animationSur :: Ani -> Surface
animationSur (Ani sur _) = sur

staticAni :: Surface -> Ani
staticAni s = Ani s (\_ -> staticAni s)

dynamicAny :: (Word32 -> Surface -> Surface) -> Surface -> Ani
dynamicAny f sur = Ani sur $ \delta -> dynamicAny f (f delta sur)

data Animated = Animated
    { animatedAni  :: Ani
    , animatedPos  :: Point
    , animatedDim  :: Dimension
    , animatedBBox :: Path
    }

instance TimeStep Animated where
    step f p@(Animated {animatedAni = ani}) =
        p {animatedAni = applyAni ani f}

instance HasSurface Animated where
    surface (Animated {animatedAni = ani}) = animationSur ani

instance Object Animated where
    rect (Animated {animatedPos = (x, y), animatedDim = (w, h)}) = Rect x y w h
    bbox (Animated {animatedPos = pos, animatedBBox = box}) =
        map (translate pos) box
