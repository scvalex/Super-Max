module Game where

import Graphics.UI.SDL.Events
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
updateGame _ delta g = return $ step delta g