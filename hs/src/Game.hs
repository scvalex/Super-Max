module Game where

import Types
import Level
import Player

data Game = Game
    { gamePlayer :: Player
    , gameActDir :: [Direction]
    , gameLevel  :: Level
    }

instance Step Game where
    step delta ev g@(Game {gamePlayer = p, gameLevel = lvl}) = do
        p'   <- step delta ev p
        lvl' <- step delta ev lvl
        return $ g {gamePlayer = p', gameLevel = lvl'}
