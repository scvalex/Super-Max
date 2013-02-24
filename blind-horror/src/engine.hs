{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Monoid ( Monoid(..) )
import Game.Engine ( GameEvent(..), play, quitGame, modifyGameState
                   , Picture(..), Color(..)
                   , Event(..), SDLKey(..), Keysym(..) )
import Text.Printf ( printf )

main :: IO ()
main = do
    play (640, 480) 10 (0 :: Int, 0 :: Double) drawRects handleEvent
  where
    drawRects (i, time) =
        mconcat [ Translate (10 * fromIntegral (i `mod` 40)) 50 $
                  Color (RGBA 255 0 0 255) $
                  FilledRectangle 0 0 10 10
                , Translate 30 (10 * fromIntegral (i `mod` 30)) $
                  Color (RGBA 0 255 0 255) $
                  FilledRectangle 0 0 10 10
                , Translate 100 100 $
                  SizedText 60 (printf "Test: %.2f" time)
                ]

    handleEvent (Tick (_, delta)) = do
        modifyGameState (\(ticks, time) -> (ticks + 1, time + delta))
    handleEvent (InputEvent (KeyUp (Keysym {symKey = SDLK_ESCAPE}))) =
        quitGame
    handleEvent _ = do
        return ()
