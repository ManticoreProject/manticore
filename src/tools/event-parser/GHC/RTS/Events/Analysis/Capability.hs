module GHC.RTS.Events.Analysis.Capability
  ( capabilityThreadPoolMachine
  , capabilityThreadRunMachine
  , capabilityThreadIndexer
  , capabilityTaskPoolMachine
  , capabilityTaskOSMachine
  )
 where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Data.Map (Map)
import qualified Data.Map as M

-- | This state machine tracks threads residing on capabilities.
-- Each thread can only reside on one capability, but can be migrated between
-- them.
capabilityThreadPoolMachine :: Machine (Map ThreadId Int) CapEvent
capabilityThreadPoolMachine = Machine
  { initial = M.empty
  , final   = const False
  , alpha   = capabilityThreadPoolMachineAlpha
  , delta   = capabilityThreadPoolMachineDelta
  }
 where
  capabilityThreadPoolMachineAlpha capEvent = case spec . ce_event $ capEvent of
     (CreateThread _)    -> True
     (StopThread _ _)    -> True
     (MigrateThread _ _) -> True
     _                   -> False

  capabilityThreadPoolMachineDelta mapping capEvent = do
    capId <- ce_cap capEvent
    case spec . ce_event $ capEvent of
      (CreateThread threadId)              -> insertThread threadId capId mapping
      (StopThread threadId ThreadFinished) -> deleteThread threadId mapping
      (StopThread _ _)                     -> Just mapping
      (MigrateThread threadId capId')      -> deleteThread threadId mapping >>=
                                                insertThread threadId capId'
      _                                    -> Nothing
   where
    insertThread :: ThreadId -> Int -> Map ThreadId Int -> Maybe (Map ThreadId Int)
    insertThread threadId capId m
      | threadId `M.member` m = Nothing -- The thread already exists
      | otherwise             = Just $ M.insert threadId capId m

    deleteThread :: ThreadId -> Map ThreadId Int -> Maybe (Map ThreadId Int)
    deleteThread threadId m
      | threadId `M.notMember` m = Nothing -- The thread doesn't exist
      | otherwise                = Just $ M.delete threadId m

-- | This state machine tracks threads running on capabilities, only one thread
-- may run on a capability at a time.
capabilityThreadRunMachine :: Machine (Map Int ThreadId) CapEvent
capabilityThreadRunMachine = Machine
  { initial = M.empty
  , final   = const False
  , alpha   = threadRunAlpha
  , delta   = threadRunDelta
  }
 where
  threadRunAlpha capEvent = case spec . ce_event $ capEvent of
    -- TODO: can threads be migrated while they are running?
    -- TODO: take into account paused threads
    (RunThread _)     -> True
    (StopThread _ _ ) -> True
    _                 -> False

  -- The indexer fails if a thread is inserted where one already exists,
  -- or if a thread is deleted that doesn't exist.
  threadRunDelta mapping e = do
    capId <- ce_cap e
    case spec . ce_event $ e of
      (RunThread threadId)     -> runThread capId threadId mapping
      (StopThread threadId _ ) -> stopThread threadId mapping
      _                        -> Just mapping
   where
    runThread :: Int -> ThreadId -> Map Int ThreadId -> Maybe (Map Int ThreadId)
    runThread capId threadId m
      | capId `M.member` m        = Nothing -- A thread is already on this cap
      | threadId `elem` M.elems m = Nothing -- This thread is already on a cap
      | otherwise                 = Just $ M.insert capId threadId m
    stopThread :: ThreadId -> Map Int ThreadId -> Maybe (Map Int ThreadId)
    stopThread threadId m
      | notElem threadId . M.elems $ m = Nothing -- The thread doesn't exist
      | otherwise                      = Just $ M.filter (/= threadId) m

capabilityThreadIndexer :: Map Int ThreadId -> CapEvent -> Maybe ThreadId
capabilityThreadIndexer m capEvent = case spec . ce_event $ capEvent of
  (CreateSparkThread threadId)  -> Just threadId
  (CreateThread threadId)       -> Just threadId
  (RunThread threadId)          -> Just threadId
  (StopThread threadId _)       -> Just threadId
  (ThreadRunnable threadId)     -> Just threadId
  (MigrateThread threadId _)    -> Just threadId
  (WakeupThread threadId capId) -> if Just capId == ce_cap capEvent
                                   then Just threadId
                                   else Nothing
  _                             -> mThreadId
 where
  mThreadId = ce_cap capEvent >>= (\capId -> M.lookup capId m)

-- | This state machine tracks Haskell tasks, represented by TaskId,
-- residing on capabilities.
-- Each Haskell task can only reside on one capability, but can be migrated
-- between them.
capabilityTaskPoolMachine :: Machine (Map TaskId Int) CapEvent
capabilityTaskPoolMachine = Machine
  { initial = M.empty
  , final   = const False
  , alpha   = capabilityTaskPoolMachineAlpha
  , delta   = capabilityTaskPoolMachineDelta
  }
 where
  capabilityTaskPoolMachineAlpha capEvent = case spec . ce_event $ capEvent of
     TaskCreate{}  -> True
     TaskDelete{}  -> True
     TaskMigrate{} -> True
     _             -> False

  capabilityTaskPoolMachineDelta mapping capEvent = do
    case spec . ce_event $ capEvent of
      TaskCreate {taskId, cap}           -> insertTask taskId cap mapping
      TaskDelete {taskId}                -> deleteTask taskId Nothing mapping
      TaskMigrate {taskId, cap, new_cap} ->
        deleteTask taskId (Just cap) mapping >>=
          insertTask taskId new_cap
      _                                  -> Nothing
   where
    insertTask :: TaskId -> Int -> Map TaskId Int
               -> Maybe (Map TaskId Int)
    insertTask taskId cap m
      | taskId `M.member` m = Nothing  -- The task already exists.
      | otherwise           = Just $ M.insert taskId cap m

    deleteTask :: TaskId -> Maybe Int -> Map TaskId Int
               -> Maybe (Map TaskId Int)
    deleteTask taskId expectedcap m
      | Just oldcap <- M.lookup taskId m
      , maybe True (==oldcap) expectedcap
      = Just $ M.delete taskId m
      | otherwise
      = Nothing  -- The task doesn't exist, or does but with an unexpected cap.

-- | This state machine tracks Haskell tasks (represented by the KernelThreadId
-- of their OS thread) residing on capabilities and additionally
-- tracks the (immutable) assignment of OS thread ids (KernelThreadId)
-- to tasks ids (TaskId).
-- Each Haskell task can only reside on one capability, but can be migrated
-- between them.
--
-- Invariant for the @(Map KernelThreadId Int, Map TaskId KernelThreadId)@ 
-- type: the second map is an injection (verified by the machine 
-- in 'insertTaskOS') and the following sets are equal: 
-- keys of the fist map and values of the second
-- (follows from the construction of the maps by the machine).
--
-- The machine verifies as much as 'capabilityTaskPoolMachine' and additionally
-- the data invariant, and offers a richer verification profile.
capabilityTaskOSMachine :: Machine (Map KernelThreadId Int,
                                    Map TaskId KernelThreadId)
                                   CapEvent
capabilityTaskOSMachine = Machine
  { initial = (M.empty, M.empty)
  , final   = const False
  , alpha   = capabilityTaskOSMachineAlpha
  , delta   = capabilityTaskOSMachineDelta
  }
 where
  capabilityTaskOSMachineAlpha capEvent = case spec . ce_event $ capEvent of
     TaskCreate{}  -> True
     TaskDelete{}  -> True
     TaskMigrate{} -> True
     _             -> False

  capabilityTaskOSMachineDelta mapping capEvent = do
    case spec . ce_event $ capEvent of
      TaskCreate {taskId, cap, tid} -> insertTaskOS taskId cap tid mapping
      TaskDelete {taskId}           -> deleteTaskOS taskId mapping
      TaskMigrate {taskId, new_cap} -> migrateTaskOS taskId new_cap mapping
      _                             -> Nothing
   where
    insertTaskOS :: TaskId -> Int -> KernelThreadId
                 -> (Map KernelThreadId Int, Map TaskId KernelThreadId)
                 -> Maybe (Map KernelThreadId Int, Map TaskId KernelThreadId)
    insertTaskOS taskId cap tid (m, ma)
      | taskId `M.member` ma = Nothing  -- The task already exists.
      | tid `M.member` m     = Nothing  -- The OS thread already exists.
      | otherwise            = Just (M.insert tid cap m,
                                     M.insert taskId tid ma)

    deleteTaskOS :: TaskId -> (Map KernelThreadId Int,
                               Map TaskId KernelThreadId)
                 -> Maybe (Map KernelThreadId Int, Map TaskId KernelThreadId)
    deleteTaskOS taskId (m, ma) =
      case M.lookup taskId ma of
        Nothing  -> Nothing  -- The task doesn't exist.
        Just tid -> Just (M.delete tid m,
                          M.delete taskId ma)

    migrateTaskOS :: TaskId -> Int -> (Map KernelThreadId Int,
                                       Map TaskId KernelThreadId)
                  -> Maybe (Map KernelThreadId Int, Map TaskId KernelThreadId)
    migrateTaskOS taskId new_cap (m, ma) =
      case M.lookup taskId ma of
        Nothing -> Nothing  -- The task doesn't exist.
        Just tid -> Just (M.insert tid new_cap m,
                          ma)  -- The assignment is immutable.
