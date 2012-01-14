module Blitting where

import Control.Monad
import Graphics.UI.SDL
import Control.Exception

import Game
import Types
import Level
import Exception

blitObject :: Object o => o -> Surface -> IO ()
blitObject o screen = do
    let s = surface o
        r = rect o
    res <- blitSurface s Nothing screen (Just r)
    unless res (throwIO FailedBlitting)

blitLevel :: Level -> Surface -> IO ()
blitLevel (Level {levelObjects = os}) screen =
    forM_ os $ \(LevelObject o) -> blitObject o screen

blitGame :: Game -> Surface -> IO ()
blitGame (Game {gamePlayer = p, gameLevel = lvl}) screen = do
    blitLevel lvl screen
    blitObject p screen
