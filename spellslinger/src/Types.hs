module Types (
        Direction(..), randomDirection
    ) where

import Control.Applicative ( (<$>) )
import Game.Engine ( Game, randomR )

data Direction = North | West | South | East
               deriving ( Enum, Eq, Show )

-- | A random direction.
randomDirection :: Game s Direction
randomDirection = toEnum <$> randomR (0, 3)