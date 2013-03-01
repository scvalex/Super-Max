-- | All changes to the world state happen sequentially.  For instance, first the player
-- moves, then all the NPCs move, then the player moves again, and so on.  The 'Scheduler'
-- encapsulates this behaviour.
module Scheduler (
        Scheduler, newScheduler,
        runScheduledActions, scheduleAction, dropExpiredActions
    ) where

import Data.PriorityQueue ( PriorityQueue )
import Game.Engine ( Game, getGameTick )
import qualified Data.PriorityQueue as PQ
import Text.Printf ( printf )

----------------------
-- Types
----------------------

data Action w = Action { getTag            :: String
                       , getScheduledTick  :: Int
                       , getAction         :: Game w () }

instance Show (Action w) where
    show a = printf "[%s at %d]" (getTag a) (getScheduledTick a)

instance Eq (Action w) where
    Action { getScheduledTick = n1 } == Action { getScheduledTick = n2 } =
        n1 == n2

instance Ord (Action w) where
    Action { getScheduledTick = n1 } `compare` Action {getScheduledTick = n2 } =
        n1 `compare` n2

data Scheduler w = Scheduler { getSchedule :: PriorityQueue (Action w) }

----------------------
-- Scheduler control
----------------------

-- | Apply all scheduled actions whose 'getScheduledTick' has passed.  Return the world
-- after said actions have been applied.
runScheduledActions :: Int                  -- ^ The current tick
                    -> Scheduler w          -- ^ The scheduler
                    -> Game w (Scheduler w) -- ^ The world after the actions have been applied
runScheduledActions n s =
    case PQ.extractMin (getSchedule s) of
        Just (Action { getScheduledTick = t
                     , getAction        = act }, pq)
            | t <= n -> do
                act
                runScheduledActions n (s { getSchedule = pq })
        _ ->
            return s

-- | Drop expired actions.
dropExpiredActions :: Int -> Scheduler w -> Scheduler w
dropExpiredActions n s =
    case PQ.extractMin (getSchedule s) of
        Just (Action { getScheduledTick = t }, pq) | t <= n ->
            dropExpiredActions n (s { getSchedule = pq })
        _ ->
            s

-- | A new scheduler with nothing scheduled.
newScheduler :: Scheduler w
newScheduler = Scheduler { getSchedule = PQ.empty }

-- | Run the given action on the given tick.
scheduleAction :: Scheduler w -> String -> Int -> Game w () -> Scheduler w
scheduleAction s tag tick act =
    let action = Action { getTag = tag
                        , getScheduledTick = tick
                        , getAction = act } in
    s { getSchedule = PQ.insert (getSchedule s) action }
