module Game.Input (
        InputEvent(..),

        -- * SDL events (re-export)
        Event(..), SDLKey(..), Keysym(..)
    ) where

import Graphics.UI.SDL ( Event(..), SDLKey(..), Keysym(..) )

-- | The input events a game may receive.
data InputEvent = InputEvent Event
