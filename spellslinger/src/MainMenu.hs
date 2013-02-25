module MainMenu (
        -- * State
        State, initState,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, modifyGameState, getGameState
                   , Picture(..), TextAlignment(..)
                   , GameEvent(..), Event(..), SDLKey(..), Keysym(..) )
import GlobalCommand ( GlobalCommand(..) )

----------------------
-- State
----------------------

data State = State { getItems        :: [(String, GlobalCommand)]
                   , getSelectedItem :: Int
                   }

initState :: State
initState = State { getItems = [ ("New Game", ToNewGame)
                               , ("Quit", ToQuit)
                               ]
                  , getSelectedItem = 0
                  }

----------------------
-- Callbacks
----------------------

drawState :: State -> Picture
drawState state =
    mconcat $
    map drawMenuItem (zip [0 :: Int ..] (map fst (getItems state)))
  where
    drawMenuItem (i, text) = Translate 0.5 (0.6 - (fromIntegral i) * 0.1) $
                             Text 50 CenterAligned $
                             if i == getSelectedItem state
                             then "- " ++ text ++ " -"
                             else text

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent (InputEvent (KeyDown (Keysym { symKey = SDLK_ESCAPE }))) = do
    return (Just ToQuit)
handleEvent (InputEvent (KeyDown (Keysym { symKey = SDLK_UP }))) = do
    modifyGameState (\s -> s { getSelectedItem = max 0 (getSelectedItem s - 1) })
    return Nothing
handleEvent (InputEvent (KeyDown (Keysym { symKey = SDLK_DOWN }))) = do
    modifyGameState (\s -> s { getSelectedItem = min (length (getItems s) - 1)
                                                     (getSelectedItem s + 1) })
    return Nothing
handleEvent (InputEvent (KeyDown (Keysym { symKey = key })))
    | key `elem` [SDLK_SPACE, SDLK_RETURN] = do
        state <- getGameState
        -- The index can't be wrong, unless we screwed up updating getSelectedItem.
        return (Just (snd (getItems state !! getSelectedItem state)))
handleEvent _ =
    return Nothing
