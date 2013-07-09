module Game.Engine.GL (
        draw
    ) where

import Game.Engine.GL.Drawing ( Drawing(..), Drawable(..) )

draw :: Drawing -> IO ()
draw _drawing =
    return ()
