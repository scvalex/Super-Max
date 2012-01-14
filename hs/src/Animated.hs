module Animated (Animated(..)) where

import Game

-- | A static object, which might have an animation and movements but which does
--   not responds to events.
data Animated = Animated
    { animatedAni  :: Ani
    , animatedMov  :: Mov
    , animatedDim  :: (Int, Int)
    , animatedBBox :: BBox
    }

instance Step Animated where
    step delta _ _ a@(Animated {animatedAni = ani, animatedMov = mov}) =
        return $
        a {animatedAni = applyAni ani delta, animatedMov = applyMov mov delta}

instance HasSurface Animated where
    surface (Animated {animatedAni = ani}) = aniSur ani

instance Object Animated where
    rect (Animated {animatedMov = mov, animatedDim = (w, h)}) =
        let (x, y) = movPos mov in Rect x y w h
    bbox (Animated {animatedMov = mov, animatedBBox = box}) =
        map (uncurry translate $ movPos mov) box
