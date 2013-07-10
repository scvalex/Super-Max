{-# LANGUAGE ExistentialQuantification #-}

module SuperMax.GL.Drawing (
        Drawing(..), SomeDrawable(..), Drawable(..)
    ) where

import Data.Vect.Float ( Mat4(..), idmtx )

data SomeDrawable = forall a. (Drawable a) => SomeDrawable a

data Drawing = Drawing { drawingViewMatrix :: Mat4
                       , drawingDrawables  :: [SomeDrawable]
                       }

class Drawable a where
    drawableVertices    :: a -> [(Float, Float, Float)]
    drawableColours     :: a -> [(Float, Float, Float)]
    drawableProgramName :: a -> String

    drawableModelMatrix :: a -> Mat4
    drawableModelMatrix _ = idmtx

instance Drawable SomeDrawable where
    drawableVertices (SomeDrawable d)    = drawableVertices d
    drawableColours (SomeDrawable d)     = drawableColours d
    drawableProgramName (SomeDrawable d) = drawableProgramName d
    drawableModelMatrix (SomeDrawable d) = drawableModelMatrix d