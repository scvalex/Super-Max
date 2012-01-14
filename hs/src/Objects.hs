module Objects where

import Graphics.UI.SDL (Surface, Rect(..))

import Types

-- | An animation.
data Ani = Ani Surface (Time -> Ani)

applyAni :: Ani -> Time -> Ani
applyAni (Ani _ f) fl = f fl

animationSur :: Ani -> Surface
animationSur (Ani sur _) = sur

staticAni :: Surface -> Ani
staticAni s = Ani s (\_ -> staticAni s)

-- | A movement.
data Mov = Mov Point (Time -> Mov)

applyMov :: Mov -> Time -> Mov
applyMov (Mov _ f) delta = f delta

movPos :: Mov -> Point
movPos (Mov p _) = p

staticMov :: Point -> Mov
staticMov p = Mov p (\_ -> staticMov p)

-- | A static object, which might have an animation and movements but which does
--   not responds to events.
data Animated = Animated
    { animatedAni  :: Ani
    , animatedMov  :: Mov
    , animatedDim  :: Dimension
    , animatedBBox :: Path
    }

instance Step Animated where
    step delta _ a@(Animated {animatedAni = ani, animatedMov = mov}) =
        return $
        a {animatedAni = applyAni ani delta, animatedMov = applyMov mov delta}

instance HasSurface Animated where
    surface (Animated {animatedAni = ani}) = animationSur ani

instance Object Animated where
    rect (Animated {animatedMov = mov, animatedDim = (w, h)}) =
        let (x, y) = movPos mov in Rect x y w h
    bbox (Animated {animatedMov = mov, animatedBBox = box}) =
        map (translate $ movPos mov) box
