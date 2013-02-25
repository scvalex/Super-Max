module Menu (
        -- * State
        State, initState,

        -- * Callbacks
        drawState, handleEvent
    ) where

import Game.Engine ( Game, GameEvent
                   , Picture )

data State = State

initState :: State
initState = State

----------------------
-- Callbacks
----------------------

drawState :: State -> Picture
drawState _ = undefined

handleEvent :: GameEvent -> Game State ()
handleEvent _ = undefined
