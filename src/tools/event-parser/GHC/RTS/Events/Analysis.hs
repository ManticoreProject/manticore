module GHC.RTS.Events.Analysis
  ( Machine (..)
  , validate
  , validates
  , simulate
  , Profile (..)
  , profile
  , profileIndexed
  , profileRouted
  , extractIndexed
  , refineM
  , profileM
  , indexM
  , toList
  , toMaybe
  , Process (..)
  , routeM
  )
 where

import GHC.RTS.Events

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (rights)

--------------------------------------------------------------------------------
-- | This is based on a simple finite state machine hence the names `delta`
-- for the state transition function.
-- Since states might be more than simple pattern matched constructors, we
-- use `finals :: state -> Bool`, rather than `Set state`, to indicate that
-- the machine is in some final state. Similarly for `alpha`, which
-- indicates the alphabet of inputs to a machine.
-- The function `delta` returns `Maybe` values, where `Nothing`
-- indicates that no valid transition is possible: ie, there has been an
-- error.
data Machine s i = Machine
  { initial :: s                 -- ^ Initial state
  , final   :: s -> Bool         -- ^ Valid final states
  , alpha   :: i -> Bool         -- ^ Valid input alphabet
  , delta   :: s -> i -> Maybe s -- ^ State transition function
  }

-- | This machine always accepts, never terminates, and always has unit state.
unitMachine :: Machine () i
unitMachine = Machine
  { initial  = ()
  , final    = const False
  , alpha    = const True
  , delta    = (\s i -> Just ())
  }

-- | The `step` function runs a machine in a state against a single input.
-- The state remains fixed once a final state is encountered. The
-- result is `Left state input` if some `state` failed for an `ìnput`, and
-- `Right state` for a successful state.
step :: Machine s i -> s -> i -> Either (s, i) s
step m s i
  | final m s = Right s
  | alpha m i = case delta m s i of
      Just s' -> Right s'
      Nothing -> Left (s, i)
  | otherwise = Right s

-- | The `validate` function takes a machine and a list of inputs. The machine
-- is started from its initial state and run against the inputs in turn.
-- It returns the state and input on failure, and just the state on success.
validate :: Machine s i -> [i] -> Either (s, i) s
validate m = foldl (>>=) (Right (initial m)) . map (flip (step m))

-- | This function is similar to `validate`, but outputs each intermediary
-- state as well. For an incremental version, use `simulate`.
validates :: Machine s i -> [i] -> [Either (s, i) s]
validates m = scanl (>>=) (Right (initial m)) . map (flip (step m))

--------------------------------------------------------------------------------
-- A Process is a list of successful values, followed by an error if one
-- occured. This captures the idea that a computation may produce a list of
-- elements before possibly failing. This gives us an incremental interface
-- to data processed from machine transitions.
data Process e a
  = Done
  | Fail e
  | Prod a (Process e a)
 deriving Show

toList :: Process e a -> [a]
toList (Fail _)    = []
toList Done        = []
toList (Prod a as) = a : toList as

toMaybe :: Process e a -> Maybe e
toMaybe (Fail e)    = Just e
toMaybe Done        = Nothing
toMaybe (Prod _ as) = toMaybe as

-- | A machine can be analysed while it is accepting input in order to extract
-- some information. This function takes a machine and a function that extracts
-- data and produces output. On failure, the machine state and input are
-- produced. Note that when an input is not in the machine's alphabet,
-- then there is no transition, and so no output is produced in response
-- to that input.
analyse :: Machine s i          -- ^ The machine used
        -> (s -> i -> Maybe o)  -- ^ An extraction function that may produce output
        -> [i]                  -- ^ A list of input
        -> Process (s, i) o     -- ^ A process that produces output
analyse machine extract is = go (initial machine) is
 where
  -- go :: s -> [i] -> Process (s, i) o
  go _ [] = Done
  go s (i:is)
    | final machine s = Done
    | alpha machine i =
        case delta machine s i of
          Nothing -> Fail (s, i)
          Just s' ->
            case extract s i of
              Nothing -> go s' is
              Just o  -> Prod o (go s' is)
    | otherwise = go s is

-- | Machines sometimes need to operate on coarser input than they are defined
-- for. This function takes a function that refines input and a machine that
-- works on refined input, and produces a machine that can work on coarse input.
refineM :: (i -> j) -> Machine s j -> Machine s i
refineM refine machine = Machine
  { initial = initial machine
  , final   = final machine
  , alpha   = alpha machine . refine
  , delta   = \s -> delta machine s . refine
  }

--------------------------------------------------------------------------------
-- | This function produces a process that outputs all the states that a
-- machine goes through.
simulate :: Machine s i -> [i] -> Process (s, i) (s, i)
simulate machine = analyse machine (\s i -> delta machine s i >>= \s' -> return (s', i))

--------------------------------------------------------------------------------
-- | A state augmented by Timestamp information is held in `profileState`.
-- When the state changes, `profileMap` stores a map between each state
-- and its cumulative time.
data Profile s = Profile
  { profileState :: s               -- ^ The current state
  , profileTime  :: Timestamp       -- ^ The entry time of the state
  } deriving (Show)

-- | This function takes a machine and profiles its state.
profileM :: (i -> Timestamp)
         -> Machine s i
         -> Machine (Profile s) i
profileM timer machine = Machine
  { initial = Profile (initial machine) 0
  , final   = final machine . profileState
  , alpha   = alpha machine
  , delta   = profileMDelta
  }
 where
  profileMDelta (Profile s _) i = do
    s' <- delta machine s i
    return $ Profile s' (timer i)

-- | extractProfile returns the state, the time this state was made,
-- and the time spent in this state.
extractProfile :: (i -> Timestamp)                -- ^ Extracts current timestamp
               -> Profile s                       -- ^ A profiled state
               -> i                               -- ^ Some input
               -> Maybe (s, Timestamp, Timestamp) -- ^ (state, currentTime, elapsedTime)
extractProfile timer p i = Just (profileState p, profileTime p, timer i - profileTime p)

profile :: Machine s i       -- ^ A machine to profile
        -> (i -> Timestamp)  -- ^ Converts input to timestamps
        -> [i]               -- ^ The list of input
        -> Process (Profile s, i) (s, Timestamp, Timestamp)
profile machine timer =
  analyse (profileM timer machine)
          (extractProfile timer)

profileIndexed :: Ord k
               => Machine s i
               -> (i -> Maybe k)
               -> (i -> Timestamp)
               -> [i]
               -> Process (Map k (Profile s), i) (k, (s, Timestamp, Timestamp))
profileIndexed machine index timer =
  analyse (indexM index (profileM timer machine))
          (extractIndexed (extractProfile timer) index)

extractIndexed :: Ord k => (s -> i -> Maybe o) -> (i -> Maybe k) -> (Map k s -> i -> Maybe (k, o))
extractIndexed extract index m i = do
  k <- index i
  s <- M.lookup k m
  o <- extract s i
  return (k, o)

-- | An indexed machine takes a function that multiplexes the input to a key
-- and then takes a machine description to an indexed machine.
indexM :: Ord k
       => (i -> Maybe k)        -- ^ An indexing function
       -> Machine s i           -- ^ A machine to index with
       -> Machine (Map k s) i   -- ^ The indexed machine
indexM index machine = Machine
  { initial = M.empty
  , final   = indexMFinal
  , alpha   = indexMAlpha
  , delta   = indexMDelta
  }
 where
  -- An indexer never reaches a final state: it is always possible that
  -- an event comes along that is accepted by a machine that is not
  -- yet in in the index.
  --
  -- An alternative view is that the indexer is in a final state if all its
  -- elements are, but this would not allow the creation of new indexes:
  --     indexMFinal m = not (M.null m) && (all (final machine) . M.elems $ m)
  indexMFinal = const False

  -- The alphabet of the indexer is that of its elements.
  indexMAlpha = alpha machine

  -- If the index is not yet in the mapping, we start a new machine in its
  -- initial state. The indexer fails if indexed state fails.
  indexMDelta m i = do
    k <- index i
    let state = fromMaybe (initial machine) (M.lookup k m)
    state' <- delta machine state i
    return $ M.insert k state' m

profileRouted :: Ord k
              => Machine s i
              -> Machine r i
              -> (r -> i -> Maybe k)
              -> (i -> Timestamp)
              -> [i]
              -> Process ((Map k (Profile s), r), i) (k, (s, Timestamp, Timestamp))
profileRouted machine router index timer =
  analyse (routeM router index (profileM timer machine))
          (extractRouted (extractProfile timer) index)

extractRouted :: Ord k => (s -> i -> Maybe o) -> (r -> i -> Maybe k) -> ((Map k s, r)  -> i -> Maybe (k, o))
extractRouted extract index (m, r) i = do
  k <- index r i
  s <- M.lookup k m
  o <- extract s i
  return (k, o)


-- | A machine can be indexed not only by the inputs, but also by the state
-- of an intermediary routing machine. This is a generalisation of indexM.
routeM :: (Ord k)
       => Machine r i
       -> (r -> i -> Maybe k)
       -> Machine s i
       -> Machine (Map k s, r) i
routeM router index machine = Machine
  { initial = (M.empty, initial router)
  , final   = routeMFinal
  , alpha   = routeMAlpha
  , delta   = routeMDelta
  }
 where
  -- As with indexers, there is no final state.
  routeMFinal = const False

  -- The alphabet is that of the router combined with the machine
  routeMAlpha i = alpha router i || alpha machine i

  routeMDelta (m, r) i = do
    r' <- if alpha router i
          then delta router r i
          else return r
    m' <- if alpha machine i
          then case index r' i of
            Just k -> do
              s' <- delta machine (fromMaybe (initial machine) (M.lookup k m)) i
              return $ M.insert k s' m
            Nothing -> return m
          else return m
    return (m', r')
