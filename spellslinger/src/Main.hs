module Main where

import Game.Engine ( Game, GameEvent, play
                   , getGameState, withAlternateGameState
                   , Picture )
import System.Random ( newStdGen )
import qualified MainMenu as MainMenu
import qualified Survival as Survival

type NpcId = Int

type Level = Int

data GameState = MainMenu MainMenu.State
               | Survival Survival.State

----------------------
-- Constants
----------------------

canvasSize :: Int
canvasSize = 1000

tps :: Int
tps = 10

----------------------
-- Main
----------------------

main :: IO ()
main = do
    putStrLn "Everything is fine."
    putStrLn "There is nothing to worry about."
    putStrLn "ohgod ohgod ohgod"
    putStrLn ""

    gen <- newStdGen

    play
        (canvasSize, canvasSize)
        tps
        (Survival (Survival.initState gen Survival.area1 1))
        drawState
        handleEvent

----------------------
-- Callbacks
----------------------

drawState :: GameState -> Picture
drawState (MainMenu state) = MainMenu.drawState state
drawState (Survival state) = Survival.drawState state

handleEvent :: GameEvent -> Game GameState ()
handleEvent ev = do
    gs <- getGameState
    case gs of
        MainMenu state -> do
            withAlternateGameState state (\state' -> MainMenu state') $
                MainMenu.handleEvent ev
        Survival state -> do
            withAlternateGameState state (\state' -> Survival state') $
                Survival.handleEvent ev
