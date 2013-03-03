-- | Dumping ground for common functions
module Common (
        -- * Drawing
        intRectangle, fromAreaCoordinates,
        bigText, mediumText, smallText,

        -- * File operations
        writeAppFile, readAppFile
    ) where

import Control.Applicative ( (<$>) )
import Game.Engine ( Picture(..), TextAlignment )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import System.FilePath ( (</>) )
import System.Directory ( getAppUserDataDirectory, createDirectoryIfMissing
                        , doesFileExist )

-- | Draw a polygon with 'Int' coordinates.
intRectangle :: Int -> Int -> Int -> Int -> Picture
intRectangle x1 y1 w h = FilledRectangle (fromIntegral x1)
                                         (fromIntegral y1)
                                         (fromIntegral w)
                                         (fromIntegral h)

-- | Convert a picture in area coordinates to one in drawing coordinates.
fromAreaCoordinates :: (Int, Int, Int, Int) -> Picture -> Picture
fromAreaCoordinates (x1, y1, x2, y2) =
    Translate (fromIntegral x1) (fromIntegral y1) .
    Scale (1.0 / fromIntegral (x2 - x1)) (1.0 / fromIntegral (y2 - y1))

-- | Text with fixed sizes
bigText, mediumText, smallText :: TextAlignment -> String -> Picture
bigText    = Text 40
mediumText = Text 30
smallText  = Text 20

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
