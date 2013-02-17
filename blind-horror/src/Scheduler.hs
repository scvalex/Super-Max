-- | All changes to the world state happen sequentially.  For instance, first the player
-- moves, then all the NPCs move, then the player moves again, and so on.  The 'Scheduler'
-- encapsulates this behaviour.
module Scheduler (
        Scheduler, newScheduler,
        runScheduledActions, scheduleAction, dropExpiredActions
    ) where

import Data.PriorityQueue ( PriorityQueue )
import qualified Data.PriorityQueue as PQ

----------------------
-- Types
----------------------

data ScheduledAction w = UpdateWorld { getScheduledTick  :: Int
                                     , getUpdateFunction :: w -> w }

instance Eq (ScheduledAction w) where
    UpdateWorld { getScheduledTick = n1 } == UpdateWorld { getScheduledTick = n2 } = n1 == n2

instance Ord (ScheduledAction w) where
    UpdateWorld { getScheduledTick = n1 } `compare` UpdateWorld {getScheduledTick = n2 } = n1 `compare` n2

data Scheduler w = Scheduler { getSchedule :: PriorityQueue (ScheduledAction w) }

----------------------
-- Scheduler control
----------------------

-- | Apply all scheduled actions whose 'getScheduledTick' has passed.  Return the world
-- after said actions have been applied.
runScheduledActions :: Int         -- ^ The current tick
                    -> w           -- ^ The initial state of the world
                    -> Scheduler w -- ^ The scheduler
                    -> w           -- ^ The world after the actions have been applied
runScheduledActions n w s =
    case PQ.extractMin (getSchedule s) of
        Just (UpdateWorld { getScheduledTick = t
                          , getUpdateFunction = update }, pq)
            | t <= n ->
                runScheduledActions n (update w) (s { getSchedule = pq })
        _ ->
            w

-- | Drop expired actions.
dropExpiredActions :: Int -> Scheduler w -> Scheduler w
dropExpiredActions n s =
    case PQ.extractMin (getSchedule s) of
        Just (UpdateWorld { getScheduledTick = t }, pq) | t <= n ->
            dropExpiredActions n (s { getSchedule = pq })
        _ ->
            s

-- | A new scheduler with nothing scheduled.
newScheduler :: Scheduler w
newScheduler = Scheduler { getSchedule = PQ.empty }

-- | Run the given action on the given tick.
scheduleAction :: Scheduler w -> Int -> (w -> w) -> Scheduler w
scheduleAction s n update =
    let act = UpdateWorld { getScheduledTick = n, getUpdateFunction = update } in
    s { getSchedule = PQ.insert (getSchedule s) act }
