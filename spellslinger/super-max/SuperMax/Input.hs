module SuperMax.Input (
        InputEvent(..), KeyEvent(..), Key(..),

        -- * GLFW events
        fromGlfwKeyEvent
    ) where

import Control.Applicative ( (<$>) )
import Debug.Trace ( trace )
import qualified Graphics.UI.GLFW as GLFW
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

-- | Convert a 'GLFW.Key' and a 'Bool' into the equivalent 'InputEvent' if it exists.  The
-- function registered with 'GLFW.setKeyCallback' gets such values.
fromGlfwKeyEvent :: GLFW.Key -> Bool -> Maybe InputEvent
fromGlfwKeyEvent key True  = InputEvent . KeyPress <$> fromGlfwKey key
fromGlfwKeyEvent key False = InputEvent . KeyRelease <$> fromGlfwKey key

fromGlfwKey :: GLFW.Key -> Maybe Key
fromGlfwKey GLFW.KeySpace     = Just KeySpace
fromGlfwKey GLFW.KeyEsc       = Just KeyEsc
fromGlfwKey GLFW.KeyUp        = Just KeyUp
fromGlfwKey GLFW.KeyDown      = Just KeyDown
fromGlfwKey GLFW.KeyLeft      = Just KeyLeft
fromGlfwKey GLFW.KeyRight     = Just KeyRight
fromGlfwKey GLFW.KeyTab       = Just KeyTab
fromGlfwKey GLFW.KeyEnter     = Just KeyEnter
fromGlfwKey GLFW.KeyBackspace = Just KeyBackspace
fromGlfwKey GLFW.KeyInsert    = Just KeyInsert
fromGlfwKey GLFW.KeyDel       = Just KeyDel
fromGlfwKey (GLFW.CharKey ch) = Just (CharKey ch)
fromGlfwKey key =
    trace (printf "Unknown keysym: %s" (show key)) $
    Nothing
