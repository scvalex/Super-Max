module Main where

import Game.Engine ( Game, GameEvent, play, quitGame
                   , modifyGameState, getGameState, withAlternateGameState
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
        start
        drawState
        handleEvent

start :: Game FullState ()
start = do
    -- Actually start the MainMenu.
    Nothing <- inMainMenu (MainMenu.start >> return Nothing)
    return ()

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
    (_, gs) <- getGameState
    mcommand <- case gs of
        MainMenu _ -> inMainMenu (MainMenu.handleEvent ev)
        Survival _ -> inSurvival (Survival.handleEvent ev)
    handleGlobalCommand mcommand
  where
    handleGlobalCommand :: Maybe GlobalCommand -> Game FullState ()
    handleGlobalCommand Nothing =
        return ()
    handleGlobalCommand (Just ToContinue) = do
        (dims, _) <- getGameState
        modifyGameState (\_ -> (dims, Survival Survival.initState))
        Nothing <- inSurvival (Survival.start Nothing >> return Nothing)
        return ()
    handleGlobalCommand (Just ToNewGame) = do
        (dims, _) <- getGameState
        modifyGameState (\_ -> (dims, Survival Survival.initState))
        Nothing <- inSurvival (Survival.start (Just 1) >> return Nothing)
        return ()
    handleGlobalCommand (Just ToMainMenu) = do
        (dims, _) <- getGameState
        modifyGameState (\_ -> (dims, MainMenu MainMenu.initState))
        Nothing <- inMainMenu (MainMenu.start >> return Nothing)
        return ()
    handleGlobalCommand (Just ToQuit) =
        quitGame

-- | Lift an action in the 'Survival' state to one in the 'FullState'.
inSurvival :: Game Survival.State (Maybe a) -> Game FullState (Maybe a)
inSurvival act = do
    (dims, _) <- getGameState
    withAlternateGameState
        (\state -> case state of
              (_, Survival survivalState) -> Just survivalState
              _                           -> Nothing)
        (\state' -> (dims, Survival state'))
        act

-- | Lift an action in the 'MainMenu' state to one in the 'FullState'.
inMainMenu :: Game MainMenu.State (Maybe a) -> Game FullState (Maybe a)
inMainMenu act = do
    (dims, _) <- getGameState
    withAlternateGameState
        (\state -> case state of
              (_, MainMenu menuState) -> Just menuState
              _                       -> Nothing)
        (\state' -> (dims, MainMenu state'))
        act
