-- | All changes to the world state happen sequentially.  For instance, first the player
-- moves, then all the NPCs move, then the player moves again, and so on.  The 'Scheduler'
-- encapsulates this behaviour.
module Scheduler (
        Scheduler, newScheduler,
        ScheduledAction(..), runScheduledActions
    ) where

import Data.PriorityQueue ( PriorityQueue )
import qualified Data.PriorityQueue as PQ

data ScheduledAction w = UpdateWorld { getScheduledTick  :: Int
                                     , getUpdateFunction :: w -> w }

instance Eq (ScheduledAction w) where
    UpdateWorld { getScheduledTick = n1 } == UpdateWorld { getScheduledTick = n2 } = n1 == n2

instance Ord (ScheduledAction w) where
    UpdateWorld { getScheduledTick = n1 } `compare` UpdateWorld {getScheduledTick = n2 } = n1 `compare` n2

data Scheduler w = Scheduler { getSchedule :: PriorityQueue (ScheduledAction w) }

-- | Apply all scheduled actions whose 'getScheduledTick' has passed.
runScheduledActions :: Int              -- ^ The current tick
                    -> w                -- ^ The initial state of the world
                    -> Scheduler w      -- ^ The scheduler
                    -> (w, Scheduler w) -- ^ The world and scheduler after the actions
                                        -- have been applied
runScheduledActions n w s =
    case PQ.extractMin (getSchedule s) of
        Just (UpdateWorld { getScheduledTick = t
                          , getUpdateFunction = update }, pq)
            | t <= n ->
                runScheduledActions n (update w) (s { getSchedule = pq })
        _ ->
            (w, s)

newScheduler :: Scheduler w
newScheduler = Scheduler { getSchedule = PQ.empty }
