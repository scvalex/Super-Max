-- | Dumping ground for common functions
module Common (
        -- * Drawing
        intRectangle, fromAreaCoordinates,
        smallText,

        -- * File operations
        writeAppFile, readAppFile
    ) where

import Control.Applicative ( (<$>) )
import Data.Vect.Float ( Mat4(..), Vec4(..) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import SuperMax ( Drawing(..)
                , Vertex(..), Colour
                , Text(..) )
import System.FilePath ( (</>) )
import System.Directory ( getAppUserDataDirectory, createDirectoryIfMissing
                        , doesFileExist )

-- | Draw a polygon with 'Int' coordinates.
intRectangle :: Colour -> Int -> Int -> Int -> Int -> [Vertex]
intRectangle col x0 y0 w0 h0 =
    let x = fromIntegral x0
        y = fromIntegral y0
        w = fromIntegral w0
        h = fromIntegral h0
    in [ ColourVertex (x,     y,     0.1) col  --    X
       , ColourVertex (x + w, y,     0.1) col  --   /|
       , ColourVertex (x + w, y + h, 0.1) col  --  X-X

       , ColourVertex (x + w, y + h, 0.1) col  --  X-X
       , ColourVertex (x,     y + h, 0.1) col  --  |/
       , ColourVertex (x,     y,     0.1) col  --  X
       ]

-- | Convert a drawing in area coordinates to one in drawing coordinates.  This sets the
-- 'drawingViewMatrix'.
fromAreaCoordinates :: (Int, Int, Int, Int) -> Drawing -> Drawing
fromAreaCoordinates (x1, y1, x2, y2) drawing =
    let scaleX = 1.0 / fromIntegral (x2 - x1 + 1)
        scaleY = 1.0 / fromIntegral (y2 - y1 + 1)
        translateX = 0.0 - fromIntegral x1
        translateY = 0.0 - fromIntegral y1
    in drawing { drawingViewMatrix = Mat4 (Vec4 scaleX 0.0    0.0 translateX)
                                          (Vec4 0.0    scaleY 0.0 translateY)
                                          (Vec4 0.0    0.0    1.0 0.0)
                                          (Vec4 0.0    0.0    0.0 1.0) }

smallText :: Float -> Float -> String -> Text
smallText x y text =
    Text { textFontName = "holstein"
         , textPosition = (x, y)
         , textSize     = 0.1
         , textText     = text
         }

-- | Write a file to the application directory.
writeAppFile :: FilePath -> String -> IO ()
writeAppFile fn contents = do
    dir <- getAppUserDataDirectory "spellslinger"
    createDirectoryIfMissing False dir
    writeFile (dir </> fn) contents

-- | Read a file from the application directory.  Uses strict IO.
readAppFile :: FilePath -> IO (Maybe String)
readAppFile fn = do
    dir <- getAppUserDataDirectory "spellslinger"
    let ffn = dir </> fn
    ok <- doesFileExist ffn
    if ok
        then Just . BL.unpack . BL.fromChunks . (:[]) <$> BS.readFile ffn
        else return Nothing
