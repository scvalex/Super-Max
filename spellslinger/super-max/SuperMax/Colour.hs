{-# LANGUAGE DeriveDataTypeable #-}

module SuperMax.Colour (
        Colour(..), white, black, greyN,
        toHexString, fromHexString, toRGBTuple
    ) where

import Data.Char ( digitToInt )
import Data.Typeable ( Typeable )
import Text.Printf ( printf )

data Colour = RGB
    { colourRed   :: Float
    , colourGreen :: Float
    , colourBlue  :: Float
    } deriving ( Eq, Read, Show, Typeable )

white, black :: Colour
white = RGB 1.0 1.0 1.0
black = RGB 0.0 0.0 0.0

greyN :: Float -> Colour
greyN c = RGB c c c

toRGBTuple :: Colour -> (Float, Float, Float)
toRGBTuple col = (colourRed col, colourGreen col, colourBlue col)

-- | Convert a 'Colour' to a string like @#00ff00@.
toHexString :: Colour -> String
toHexString col = printf "#%02x%02x%02x"
                               (toWord8 (colourRed col))
                               (toWord8 (colourGreen col))
                               (toWord8 (colourBlue col))
  where
    toWord8 :: Float -> Int
    toWord8 c = floor (c * 255.0)

-- | Convert a string like @#00ff00@ to a 'Colour'.
fromHexString :: String -> Maybe Colour
fromHexString ('#':r1:r2:g1:g2:b1:b2:_) =
    let r = fromWord8 (digitToInt r1 * 16 + digitToInt r2)
        g = fromWord8 (digitToInt g1 * 16 + digitToInt g2)
        b = fromWord8 (digitToInt b1 * 16 + digitToInt b2) in
    Just (RGB r g b)
  where
    fromWord8 :: Int -> Float
    fromWord8 d = fromIntegral d / 255.0
fromHexString _col =
    Nothing
