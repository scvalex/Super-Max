module Main where

import Game.Engine ( Game, GameEvent, play, quitGame
                   , setGameState, getGameState, withAlternateGameState
                   , Picture(..) )
import GlobalCommand ( GlobalCommand(..) )
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

    play
        (canvasSize, canvasSize)
        tps
        (MainMenu MainMenu.initState)
        drawState
        handleEvent

----------------------
-- Callbacks
----------------------

drawState :: GameState -> Picture
drawState gs =
    -- We draw on a (x = 0.0 -- 1.0, y = 0.0 -- 1.0) sized canvas.
    Scale (fromIntegral canvasSize) (fromIntegral canvasSize) $
    -- Now, draw the scene
    case gs of
        MainMenu state -> MainMenu.drawState state
        Survival state -> Survival.drawState state

handleEvent :: GameEvent -> Game GameState ()
handleEvent ev = do
    gs <- getGameState
    mcommand <- case gs of
        MainMenu state -> do
            withAlternateGameState state (\state' -> MainMenu state') $
                MainMenu.handleEvent ev
        Survival state -> do
            withAlternateGameState state (\state' -> Survival state') $
                Survival.handleEvent ev
    handleGlobalCommand mcommand
  where
    handleGlobalCommand :: Maybe GlobalCommand -> Game GameState ()
    handleGlobalCommand Nothing =
        return ()
    handleGlobalCommand (Just ToNewGame) =
        setGameState . Survival =<< (Survival.initState 1)
    handleGlobalCommand (Just ToMainMenu) =
        setGameState (MainMenu MainMenu.initState)
    handleGlobalCommand (Just ToQuit) =
        quitGame
