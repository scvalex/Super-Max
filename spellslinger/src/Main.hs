module Main where

import Data.Dynamic ( Dynamic )
import Data.Map ( Map )
import Data.Vect.Float ( Mat4(..), Vec4(..) )
import GlobalCommand ( GlobalCommand(..) )
import qualified Data.Map as M
import qualified HighScores as HighScores
import qualified MainMenu as MainMenu
import qualified Survival as Survival
import SuperMax ( Game, InputEvent, play, quitGame
                , modifyGameState, getGameState, withAlternateGameState
                , Drawing(..) )
import Debug.Trace ( trace )

type NpcId = Int

type Level = Int

type FullState = ((Int, Int), GameState)

data GameState = MainMenu MainMenu.State
               | Survival Survival.State
               | HighScores HighScores.State

----------------------
-- Constants
----------------------

tps :: Int
tps = 8

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
        "Spellslinger"
        tps
        (M.fromList [("default", ("vertex.glsl", "fragment.glsl"))])
        (M.fromList [("holstein", "holstein.png")])
        loadResources
        (\screenW screenH -> ((screenW, screenH), MainMenu MainMenu.initState))
        start
        drawState
        handleInput
        handleTick

start :: Game FullState ()
start = do
    -- Actually start the MainMenu.
    Nothing <- inMainMenu (MainMenu.start >> return Nothing)
    return ()

loadResources :: FilePath -> IO (Map String Dynamic)
loadResources resDir = MainMenu.loadResources resDir

----------------------
-- Callbacks
----------------------

drawState :: FullState -> Drawing
drawState ((_screenW, _screenH), gs) =
    -- We draw on a (x = 0.0 -- 1.0, y = 0.0 -- 1.0) sized canvas.
    let drawing = case gs of
            MainMenu state   -> MainMenu.drawState state
            Survival state   -> Survival.drawState state
            HighScores state -> HighScores.drawState state
        -- FIXME Hardcoding this ratio is ugly.
        r = 1.1
        drawing' = drawing { drawingViewMatrix = Mat4 (Vec4 r   0.0 0.0 0.0)
                                                      (Vec4 0.0 r   0.0 0.0)
                                                      (Vec4 0.0 0.0 1.0 0.0)
                                                      (Vec4 (-0.5) (-1.0) 0.0 1.0)
                           }
    in drawing'

handleInput :: InputEvent -> Game FullState ()
handleInput ev = do
    (_, gs) <- getGameState
    mcommand <- case gs of
        MainMenu _   -> inMainMenu (MainMenu.handleInput ev)
        Survival _   -> inSurvival (Survival.handleInput ev)
        HighScores _ -> inHighScores (HighScores.handleInput ev)
    handleGlobalCommand mcommand

handleTick :: Float -> Game FullState ()
handleTick tDelta = do
    trace "tick" $ return ()
    (_, gs) <- getGameState
    mcommand <- case gs of
        MainMenu _   -> inMainMenu (MainMenu.handleTick tDelta)
        Survival _   -> inSurvival (Survival.handleTick tDelta)
        HighScores _ -> inHighScores (HighScores.handleTick tDelta)
    handleGlobalCommand mcommand

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
    Nothing <- inSurvival (Survival.start (Just (1, 0)) >> return Nothing)
    return ()
handleGlobalCommand (Just ToMainMenu) = do
    (dims, _) <- getGameState
    modifyGameState (\_ -> (dims, MainMenu MainMenu.initState))
    Nothing <- inMainMenu (MainMenu.start >> return Nothing)
    return ()
handleGlobalCommand (Just ToHighScores) = do
    (dims, _) <- getGameState
    modifyGameState (\_ -> (dims, HighScores HighScores.initState))
    Nothing <- inHighScores (HighScores.start >> return Nothing)
    return ()
handleGlobalCommand (Just ToQuit) =
    quitGame

----------------------
-- Sub-state manipulations
----------------------

-- | Lift an action in the 'Survival' sub-state to one in the 'FullState'.
inSurvival :: Game Survival.State (Maybe a) -> Game FullState (Maybe a)
inSurvival act = do
    (dims, _) <- getGameState
    withAlternateGameState
        (\state -> case state of
              (_, Survival survivalState) -> Just survivalState
              _                           -> Nothing)
        (\state' -> (dims, Survival state'))
        act

-- | Lift an action in the 'MainMenu' sub-state to one in the 'FullState'.
inMainMenu :: Game MainMenu.State (Maybe a) -> Game FullState (Maybe a)
inMainMenu act = do
    (dims, _) <- getGameState
    withAlternateGameState
        (\state -> case state of
              (_, MainMenu menuState) -> Just menuState
              _                       -> Nothing)
        (\state' -> (dims, MainMenu state'))
        act

-- | Lift an action in the 'MainMenu' sub-state to one in the 'FullState'.
inHighScores :: Game HighScores.State (Maybe a) -> Game FullState (Maybe a)
inHighScores act = do
    (dims, _) <- getGameState
    withAlternateGameState
        (\state -> case state of
              (_, HighScores hsState) -> Just hsState
              _                       -> Nothing)
        (\state' -> (dims, HighScores state'))
        act
