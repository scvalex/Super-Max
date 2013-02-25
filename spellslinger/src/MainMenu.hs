module MainMenu (
        -- * State
        State, initState,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, GameEvent
                   , Picture )
import GlobalCommand ( GlobalCommand )

----------------------
-- State
----------------------

data State = State

initState :: State
initState = State

----------------------
-- Callbacks
----------------------

drawState :: State -> Picture
drawState _ = mempty

handleEvent :: GameEvent -> Game State (Maybe GlobalCommand)
handleEvent _ = return Nothing
