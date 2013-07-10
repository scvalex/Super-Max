{-# LANGUAGE ExistentialQuantification #-}

module SuperMax.GL.Drawing (
        Drawing(..), SomeDrawable(..), Drawable(..)
    ) where

data SomeDrawable = forall a. (Drawable a) => SomeDrawable a

data Drawing = Drawing { drawingScale       :: (Float, Float)
                       , drawingTranslation :: (Float, Float)
                       , drawingEntities    :: [SomeDrawable]
                       }

class Drawable a where
    drawableVertices    :: a -> [Float]
    drawableUVs         :: a -> [Float]
    drawableProgramName :: a -> String
