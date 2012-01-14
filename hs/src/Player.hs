module Player where

import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL (Rect(..))
import Prelude hiding (Either (..))

import Objects
import Types
import Common

data Direction = Left | Right
    deriving (Eq, Show)

data Player = Player
    { playerAni  :: Ani
    , playerPos  :: Point
    , playerDim  :: Dimension
    , playerBBox :: Path
    , playerDir  :: Maybe Direction
    }

velocity :: Float
velocity = 0.3

movePlayer :: Time -> Player -> Player
movePlayer _ p@(Player {playerDir = Nothing}) = p
movePlayer d p@(Player {playerDir = Just dir, playerPos = (x, y)}) =
    case dir of
        Left  -> p {playerPos = (round (fi x - fi d * velocity), y)}
        Right -> p {playerPos = (round (fi x + fi d * velocity), y)}

instance Step Player where
    step delta (KeyDown k) p = return $
        case symKey k of
            SDLK_LEFT  -> movePlayer delta $ p {playerDir = Just Left}
            SDLK_RIGHT -> movePlayer delta $ p {playerDir = Just Right}
            _          -> p
    step delta (KeyUp k) p@(Player {playerDir = Just dir})
        | (symKey k == SDLK_LEFT && dir == Left) || (symKey k == SDLK_RIGHT && dir == Right) =
          return $ p {playerDir = Nothing}
        | otherwise = return $ movePlayer delta p
    step delta _ p = return $ movePlayer delta p

instance HasSurface Player where
    surface (Player {playerAni = ani}) = animationSur ani

instance Object Player where
    rect (Player {playerPos = (x, y), playerDim = (w, h)}) = Rect x y w h
    bbox (Player {playerPos = pos, playerBBox = box}) =
        map (translate pos) box
