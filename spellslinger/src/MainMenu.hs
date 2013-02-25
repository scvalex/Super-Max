module MainMenu (
        -- * State
        State, initState,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Data.Monoid ( Monoid(..) )
import Game.Engine ( Game, GameEvent
                   , Picture )

data State = State

initState :: State
initState = State

----------------------
-- Callbacks
----------------------

drawState :: State -> Picture
drawState _ = mempty

handleEvent :: GameEvent -> Game State ()
handleEvent _ = return ()
