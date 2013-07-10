{-# LANGUAGE ExistentialQuantification #-}

module SuperMax.GL.Drawing (
        Drawing(..), Drawable(..)
    ) where

data Drawing = forall a. (Drawable a)
             => Drawing { drawingScale       :: (Float, Float)
                        , drawingTranslation :: (Float, Float)
                        , drawingEntities    :: [a]
                        }

class Drawable a where
    drawableVertices :: a -> [Float]
    drawableUVs      :: a -> [Float]
