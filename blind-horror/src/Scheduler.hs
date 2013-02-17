-- | All changes to the world state happen sequentially.  For instance, first the player
-- moves, then all the NPCs move, then the player moves again, and so on.  The 'Scheduler'
-- encapsulates this behaviour.
module Scheduler (
        ScheduledAction(..)
    ) where

data ScheduledAction w = UpdateWorld { getScheduledTick  :: Int
                                     , getUpdateFunction :: w -> w }

instance Eq (ScheduledAction w) where
    UpdateWorld { getScheduledTick = n1 } == UpdateWorld { getScheduledTick = n2 } = n1 == n2

instance Ord (ScheduledAction w) where
    UpdateWorld { getScheduledTick = n1 } `compare` UpdateWorld {getScheduledTick = n2 } = n1 `compare` n2
