module Game.Input (
        InputEvent(..), KeyEvent(..), Key(..),

        -- * SDL events
        fromSdlEvent
    ) where

import Control.Applicative ( (<$>) )
import Debug.Trace ( trace )
import Graphics.UI.SDL ( Keysym(..), SDLKey(..) )
import qualified Graphics.UI.SDL as SDL
import Text.Printf ( printf )

-- | The input events a game may receive.
data InputEvent = InputEvent KeyEvent
                deriving ( Eq, Ord, Read, Show )

data KeyEvent = KeyPress Key
              | KeyRelease Key
              deriving ( Eq, Ord, Read, Show )

data Key = CharKey Char
         | KeySpace
         | KeyEsc
         | KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
         | KeyTab
         | KeyEnter
         | KeyBackspace
         | KeyInsert
         | KeyDel
         deriving (Eq, Ord, Read, Show)

-- | Convert an 'SDL.Event' into the equivalent 'InputEvent' if it exists.
fromSdlEvent :: SDL.Event -> Maybe InputEvent
fromSdlEvent (SDL.KeyUp keysym)   = InputEvent . KeyRelease <$> fromSdlKeysym keysym
fromSdlEvent (SDL.KeyDown keysym) = InputEvent . KeyPress <$> fromSdlKeysym keysym
fromSdlEvent _                    = Nothing

fromSdlKeysym :: Keysym -> Maybe Key
fromSdlKeysym (Keysym { symKey = SDLK_SPACE })     = Just KeySpace
fromSdlKeysym (Keysym { symKey = SDLK_ESCAPE })    = Just KeyEsc
fromSdlKeysym (Keysym { symKey = SDLK_UP })        = Just KeyUp
fromSdlKeysym (Keysym { symKey = SDLK_DOWN })      = Just KeyDown
fromSdlKeysym (Keysym { symKey = SDLK_LEFT })      = Just KeyLeft
fromSdlKeysym (Keysym { symKey = SDLK_RIGHT })     = Just KeyRight
fromSdlKeysym (Keysym { symKey = SDLK_TAB })       = Just KeyTab
fromSdlKeysym (Keysym { symKey = SDLK_RETURN })    = Just KeyEnter
fromSdlKeysym (Keysym { symKey = SDLK_BACKSPACE }) = Just KeyBackspace
fromSdlKeysym (Keysym { symKey = SDLK_INSERT })    = Just KeyInsert
fromSdlKeysym (Keysym { symKey = SDLK_DELETE })    = Just KeyDel
fromSdlKeysym (Keysym { symKey = SDLK_a })         = Just (CharKey 'a')
fromSdlKeysym (Keysym { symKey = SDLK_b })         = Just (CharKey 'b')
fromSdlKeysym (Keysym { symKey = SDLK_c })         = Just (CharKey 'c')
fromSdlKeysym (Keysym { symKey = SDLK_d })         = Just (CharKey 'd')
fromSdlKeysym (Keysym { symKey = SDLK_e })         = Just (CharKey 'e')
fromSdlKeysym (Keysym { symKey = SDLK_f })         = Just (CharKey 'f')
fromSdlKeysym (Keysym { symKey = SDLK_g })         = Just (CharKey 'g')
fromSdlKeysym (Keysym { symKey = SDLK_h })         = Just (CharKey 'h')
fromSdlKeysym (Keysym { symKey = SDLK_i })         = Just (CharKey 'i')
fromSdlKeysym (Keysym { symKey = SDLK_j })         = Just (CharKey 'j')
fromSdlKeysym (Keysym { symKey = SDLK_k })         = Just (CharKey 'k')
fromSdlKeysym (Keysym { symKey = SDLK_l })         = Just (CharKey 'l')
fromSdlKeysym (Keysym { symKey = SDLK_m })         = Just (CharKey 'm')
fromSdlKeysym (Keysym { symKey = SDLK_n })         = Just (CharKey 'n')
fromSdlKeysym (Keysym { symKey = SDLK_o })         = Just (CharKey 'o')
fromSdlKeysym (Keysym { symKey = SDLK_p })         = Just (CharKey 'p')
fromSdlKeysym (Keysym { symKey = SDLK_q })         = Just (CharKey 'q')
fromSdlKeysym (Keysym { symKey = SDLK_r })         = Just (CharKey 'r')
fromSdlKeysym (Keysym { symKey = SDLK_s })         = Just (CharKey 's')
fromSdlKeysym (Keysym { symKey = SDLK_t })         = Just (CharKey 't')
fromSdlKeysym (Keysym { symKey = SDLK_u })         = Just (CharKey 'u')
fromSdlKeysym (Keysym { symKey = SDLK_v })         = Just (CharKey 'v')
fromSdlKeysym (Keysym { symKey = SDLK_w })         = Just (CharKey 'w')
fromSdlKeysym (Keysym { symKey = SDLK_x })         = Just (CharKey 'x')
fromSdlKeysym (Keysym { symKey = SDLK_y })         = Just (CharKey 'y')
fromSdlKeysym (Keysym { symKey = SDLK_z })         = Just (CharKey 'z')
fromSdlKeysym keysym =
    trace (printf "Unknown keysym: %s" (show keysym)) $
    Nothing
