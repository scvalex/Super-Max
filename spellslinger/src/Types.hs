module Types (
        Direction(..), randomDirection,
        Score
    ) where

import Control.Applicative ( (<$>) )
import SuperMax ( Game, randomR )

-- | The score is just a number.
type Score = Int

data Direction = North | West | South | East
               deriving ( Enum, Eq, Show )

-- | A random direction.
randomDirection :: Game s Direction
randomDirection = toEnum <$> randomR (0, 3)
