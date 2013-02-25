module Main where

import Game.Engine ( Game, GameEvent, play
                   , getGameState, withAlternateGameState
                   , Picture )
import System.Random ( newStdGen )
import qualified Menu as Menu
import qualified Survival as Survival

type NpcId = Int

type Level = Int

data GameState = Menu Menu.State
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
drawState (Menu state)     = Menu.drawState state
drawState (Survival state) = Survival.drawState state

handleEvent :: GameEvent -> Game GameState ()
handleEvent ev = do
    gs <- getGameState
    case gs of
        Menu state -> do
            withAlternateGameState state (\state' -> Menu state') $
                Menu.handleEvent ev
        Survival state -> do
            withAlternateGameState state (\state' -> Survival state') $
                Survival.handleEvent ev
