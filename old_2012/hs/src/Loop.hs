{-# LANGUAGE DoAndIfThenElse #-}
module Loop (loop) where

import Graphics.UI.SDL hiding (update)
import Data.Word (Word32)

loopTime :: Word32               -- ^ Millisecond step
         -> Word32               -- ^ Previous time
         -> world                -- ^ The initial world
         -> (world -> IO ())
         -> (Word32 -> Event -> world -> IO world)
         -> IO ()
loopTime step pt world draw update = do
    t <- getTicks
    let delta = t - pt
    if delta < step then do
        delay delta
        loopTime step pt world draw update
    else do
        ev <- pollEvent
        case ev of
            Quit -> return ()
            _    -> do
                world' <- update delta ev world
                draw world'
                loopTime step t world' draw update

loop :: Int                      -- ^ FPS
     -> world                    -- ^ The initial world
     -> (world -> IO ())           -- ^ The drawing function
     -> (Word32 -> Event -> world -> IO world) -- ^ The updating function
     -> IO ()
loop fps world draw update = do
    t <- getTicks
    loopTime step t world draw update
  where
    step = 1000 `div` fromIntegral fps