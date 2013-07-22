{-# LANGUAGE ExistentialQuantification #-}

module SuperMax.GL.Drawing (
        Drawing(..), SomeDrawable(..), Model(..), Drawable(..),
        Vertex(..), serializeVertex,
        Text(..)
    ) where

import Data.Default ( Default(..) )
import Data.Vect.Float ( Mat4(..), idmtx )
import SuperMax.Colour ( Colour, toRGBTuple )

data SomeDrawable = forall a. (Drawable a) => SomeDrawable a

data Drawing = Drawing { drawingViewMatrix :: Mat4
                       , drawingDrawables  :: [SomeDrawable]
                       }

instance Default Drawing where
    def = Drawing { drawingViewMatrix = idmtx
                  , drawingDrawables  = []
                  }

data Model = TriangleModel [(Vertex, Vertex, Vertex)]
           | LineModel [(Vertex, Vertex)]
             deriving ( Show, Eq )

class Drawable a where
    drawableModel :: a -> Maybe Model
    drawableModel _ = def

    drawableHudTexts :: a -> [Text]
    drawableHudTexts _ = []

    drawableProgramName :: a -> String
    drawableProgramName _ = "default"

    drawableModelMatrix :: a -> Mat4
    drawableModelMatrix _ = idmtx

instance Drawable SomeDrawable where
    drawableModel (SomeDrawable d)       = drawableModel d
    drawableHudTexts (SomeDrawable d)    = drawableHudTexts d
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

data Text = Text
    { textFontName :: String
    , textPosition :: (Float, Float)
    , textSize     :: Float
    , textText     :: String
    } deriving ( Eq, Show )
