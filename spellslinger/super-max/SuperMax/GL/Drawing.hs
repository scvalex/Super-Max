{-# LANGUAGE ExistentialQuantification #-}

module SuperMax.GL.Drawing (
        Drawing(..), SomeDrawable(..), Drawable(..),
        Vertex(..), serializeVertex
    ) where

import Data.Vect.Float ( Mat4(..), idmtx )
import SuperMax.Colour ( Colour, toRGBTuple )

data SomeDrawable = forall a. (Drawable a) => SomeDrawable a

data Drawing = Drawing { drawingViewMatrix :: Mat4
                       , drawingDrawables  :: [SomeDrawable]
                       , drawingHudText    :: [(String, Float, Float, Float, String)]
                       }

class Drawable a where
    drawableVertices    :: a -> [Vertex]
    drawableProgramName :: a -> String

    drawableModelMatrix :: a -> Mat4
    drawableModelMatrix _ = idmtx

instance Drawable SomeDrawable where
    drawableVertices (SomeDrawable d)    = drawableVertices d
    drawableProgramName (SomeDrawable d) = drawableProgramName d
    drawableModelMatrix (SomeDrawable d) = drawableModelMatrix d

data Vertex = ColourVertex
    { vertexPosition :: (Float, Float, Float)
    , vertexColour :: Colour
    } deriving ( Eq, Show )

-- FIXME Vertex should have a Storable instance.

-- | Convert a 'Vertex' into a list of 'Float's that can be sent to OpenGL.
serializeVertex :: Vertex -> [Float]
serializeVertex v =
    let (x, y, z) = vertexPosition v
        (r, g, b) = toRGBTuple (vertexColour v)
    in [x, y, z, r, g, b]
