module Game where

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
