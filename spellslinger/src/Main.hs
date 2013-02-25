module Main where

import Game.Engine ( Game, GameEvent, play, quitGame
                   , setGameState, getGameState, withAlternateGameState
                   , Picture(..) )
import GlobalCommand ( GlobalCommand(..) )
import qualified MainMenu as MainMenu
import qualified Survival as Survival

type NpcId = Int

type Level = Int

type FullState = ((Int, Int), GameState)

data GameState = MainMenu MainMenu.State
               | Survival Survival.State

----------------------
-- Constants
----------------------

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

    play
        tps
        (\screenW screenH -> ((screenW, screenH), MainMenu MainMenu.initState))
        drawState
        handleEvent

----------------------
-- Callbacks
----------------------

drawState :: FullState -> Picture
drawState ((screenW, screenH), gs) =
    -- We draw on a (x = 0.0 -- 1.0, y = 0.0 -- 1.0) sized canvas.
    let dimMin = min screenW screenH in
    Translate (fromIntegral (screenW - dimMin) / 2.0) (fromIntegral (screenH - dimMin) / 2.0) $
    Scale (fromIntegral dimMin) (fromIntegral dimMin) $
    -- Now, draw the scene
    case gs of
        MainMenu state -> MainMenu.drawState state
        Survival state -> Survival.drawState state

handleEvent :: GameEvent -> Game FullState ()
handleEvent ev = do
    (dims, gs) <- getGameState
    mcommand <- case gs of
        MainMenu state -> do
            withAlternateGameState state (\state' -> (dims, MainMenu state')) $
                MainMenu.handleEvent ev
        Survival state -> do
            withAlternateGameState state (\state' -> (dims, Survival state')) $
                Survival.handleEvent ev
    handleGlobalCommand mcommand
  where
    handleGlobalCommand :: Maybe GlobalCommand -> Game FullState ()
    handleGlobalCommand Nothing =
        return ()
    handleGlobalCommand (Just ToNewGame) = do
        (dims, _) <- getGameState
        survivalState <- Survival.initState 1
        setGameState (dims, Survival survivalState)
    handleGlobalCommand (Just ToMainMenu) = do
        (dims, _) <- getGameState
        setGameState (dims, MainMenu MainMenu.initState)
    handleGlobalCommand (Just ToQuit) =
        quitGame
