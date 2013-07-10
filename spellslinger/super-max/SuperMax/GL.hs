module SuperMax.GL (
        draw
    ) where

import SuperMax.GL.Drawing ( Drawing(..), Drawable(..) )

draw :: Drawing -> IO ()
draw _drawing =
    return ()
