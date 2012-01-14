module Game where

import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym
import Data.Word (Word32)
import Prelude hiding (Either (..))

import Types
import Level
import Objects

data Direction = Left | Right | Up | Down
    deriving (Eq, Show)

data Game = Game
    { gamePlayer :: Animated
    , gameActDir :: [Direction]
    , gameLevel  :: Level
    }

instance TimeStep Game where
    step f g@(Game {gamePlayer = p, gameLevel = lvl}) =
        g {gamePlayer = step f p, gameLevel = step f lvl}

updateGame :: Event -> Word32 -> Game -> IO Game
updateGame (KeyDown k) delta g@(Game {gamePlayer = p}) = return $
    case symKey k of
         SDLK_LEFT  -> mov (-10) 0
         SDLK_RIGHT -> mov 10 0
         SDLK_UP    -> mov 0 (-10)
         SDLK_DOWN  -> mov 0 10
         _          -> g'
  where
    g'       = step delta g
    (gx, gy) = animatedPos p
    mov x y  = g' {gamePlayer = p {animatedPos = (gx+x, gy+y)}}
updateGame _ delta g = return $ step delta g