{-# LANGUAGE CPP #-}
module Main where

import GHC.RTS.Events
import GHC.RTS.Events.Merge
import GHC.RTS.Events.Analysis
import GHC.RTS.Events.Analysis.SparkThread
import GHC.RTS.Events.Analysis.Thread
import GHC.RTS.Events.Analysis.Capability

import System.Environment
import Text.Printf
import Data.List
import Data.Either (rights)
import Data.Function
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.Exit

main = getArgs >>= command

command ["--help"] = do
    putStr usage

command ["show", file] = do
    log <- readLogOrDie file
    putStrLn $ ppEventLog log

command ["show", "threads", file] = do
    eventLog <- readLogOrDie file
    let eventTypeMap = buildEventTypeMap . eventTypes . header $ eventLog
    let capEvents = sortEvents . events . dat $ eventLog
    let mappings  = rights . validates capabilityThreadRunMachine $ capEvents
    let indexes = map (uncurry capabilityThreadIndexer) $ zip mappings capEvents
    let threadMap = M.fromListWith (++) . reverse $ zip indexes (map return capEvents)
    putStrLn "Event Types:"
    putStrLn . unlines . map ppEventType . eventTypes . header $ eventLog
    putStrLn "Thread Indexed Events:"
    putStrLn . showMap
      ((++ "\n") . show)
      (unlines . map (("  " ++) . ppEvent eventTypeMap)) $
        threadMap

command ["show", "caps", file] = do
    eventLog <- readLogOrDie file
    let eventTypeMap = buildEventTypeMap . eventTypes . header $ eventLog
    let capEvents = sortEvents . events . dat $ eventLog
    let indexes = map ce_cap capEvents
    let capMap = M.fromListWith (++) . reverse $ zip indexes (map return capEvents)
    putStrLn "Event Types:"
    putStrLn . unlines . map ppEventType . eventTypes . header $ eventLog
    putStrLn "Cap Indexed Events:"
    putStrLn . showMap
      ((++ "\n") . show)
      (unlines . map (("  " ++) . ppEvent eventTypeMap)) $
        capMap

command ["merge", out, file1, file2] = do
    log1 <- readLogOrDie file1
    log2 <- readLogOrDie file2
    let m = mergeEventLogs log1 log2
    writeEventLogToFile out m

command ["validate", "threads", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate (routeM capabilityThreadRunMachine
                                  capabilityThreadIndexer
                                  (refineM (spec . ce_event) threadMachine))
                          capEvents
    putStrLn $ showValidate (\(m, n) ->
                               "\nThread States:\n" ++ showIndexed show show m ++
                               "\nCap States:\n" ++ showIndexed show show n)
                            show result

command ["validate", "threadpool", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate capabilityThreadPoolMachine capEvents
    putStrLn $ showValidate show show result

command ["validate", "threadrun", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate capabilityThreadRunMachine capEvents
    putStrLn $ showValidate show show result

command ["validate", "taskpool", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate capabilityTaskPoolMachine capEvents
    putStrLn $ showValidate show show result

command ["validate", "tasks", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate capabilityTaskOSMachine capEvents
    putStrLn $ showValidate show show result

command ["validate", "sparks", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate
                   (routeM capabilitySparkThreadMachine capabilitySparkThreadIndexer
                     (refineM (spec . ce_event) sparkThreadMachine))
                   capEvents
    putStrLn $ showValidate show show result

command ["simulate", "threads", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = simulate (routeM capabilityThreadRunMachine
                                  capabilityThreadIndexer
                                  (refineM (spec . ce_event) threadMachine))
                          capEvents
    putStrLn . showProcess $ result

command ["simulate", "threadpool", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = simulate capabilityThreadPoolMachine capEvents
    putStrLn . showProcess $ result

command ["simulate", "threadrun", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = simulate capabilityThreadRunMachine capEvents
    putStrLn . showProcess $ result

command ["simulate", "taskpool", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = simulate capabilityTaskPoolMachine capEvents
    putStrLn . showProcess $ result

command ["simulate", "tasks", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = simulate capabilityTaskOSMachine capEvents
    putStrLn . showProcess $ result

command ["simulate", "sparks", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = simulate
              (routeM capabilitySparkThreadMachine
                  capabilitySparkThreadIndexer
                  (refineM (spec . ce_event) sparkThreadMachine))
              capEvents
    putStrLn . showProcess $ result

command ["profile", "threads", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = profileRouted
                   (refineM (spec. ce_event) threadMachine)
                   capabilityThreadRunMachine
                   capabilityThreadIndexer
                   (time . ce_event)
                   capEvents
    putStrLn . showProcess $ result

command ["profile", "sparks", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = profileRouted
                   (refineM (spec . ce_event) sparkThreadMachine)
                   capabilitySparkThreadMachine
                   capabilitySparkThreadIndexer
                   (time . ce_event)
                   capEvents
    putStrLn . showProcess $ result

command _ = putStr usage >> die "Unrecognized command"

usage = unlines $ map pad strings
 where
    align = 4 + (maximum . map (length . fst) $ strings)
    pad (x, y) = zipWith const (x ++ repeat ' ') (replicate align ()) ++ y
    strings = [ ("ghc-events --help:",                     "Display this help.")

              , ("ghc-events show <file>:",                "Pretty print an event log.")
              , ("ghc-events show threads <file>:",        "Pretty print an event log, ordered by threads.")
              , ("ghc-events show caps <file>:",           "Pretty print an event log, ordered by capabilities.")

              , ("ghc-events merge <out> <in1> <in2>:",    "Merge two event logs.")

              , ("ghc-events sparks-csv <file>:",          "Print spark information in CSV.")

              , ("ghc-events validate threads <file>:",    "Validate thread states.")
              , ("ghc-events validate threadpool <file>:", "Validate thread pool state.")
              , ("ghc-events validate threadrun <file>:",  "Validate thread running state.")
              , ("ghc-events validate tasks <file>:",      "Validate task states.")
              , ("ghc-events validate sparks <file>:",     "Validate spark thread states.")

              , ("ghc-events simulate threads <file>:",    "Simulate thread states.")
              , ("ghc-events simulate threadpool <file>:", "Simulate thread pool state.")
              , ("ghc-events simulate threadrun <file>:",  "Simulate thread running state.")
              , ("ghc-events simulate tasks <file>:",      "Simulate task states.")
              , ("ghc-events simulate sparks <file>:",     "Simulate spark thread states.")

              , ("ghc-events profile threads <file>:",     "Profile thread states.")
              , ("ghc-events profile sparks <file>:",      "Profile spark thread states.")
              ]

readLogOrDie file = do
    e <- readEventLogFromFile file
    case e of
        Left s    -> die ("Failed to parse " ++ file ++ ": " ++ s)
        Right log -> return log

#if ! MIN_VERSION_base(4,8,0)
die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)
#endif

showValidate :: (s -> String) -> (i -> String) -> Either (s, i) s -> String
showValidate showState showInput (Left (state, input)) =
  "Invalid eventlog:"
  ++ "\nState:\n" ++ ( showState state )
  ++ "\nInput:\n" ++ ( showInput input )
showValidate showState _ (Right state) =
  "Valid eventlog: " ++ ( showState state )

showProcess :: (Show e, Show a) => Process e a -> String
showProcess process =
  "Trace:\n"
  ++ (unlines . map show . toList) process
  ++ "\n"
  ++ (maybe "Valid." (("Invalid:\n" ++) . show) . toMaybe) process

showIndexed :: (k -> String) -> (v -> String) -> Map k v -> String
showIndexed showKey showValue m
  | M.null m  = "Empty map\n"
  | otherwise = "Indexed output:\n" ++
      concatMap (\(k, v) -> "Key: " ++ ( showKey k ) ++ ", Value: "
          ++ ( showValue v ) ++ "\n")
        (M.toList m)

showMap :: Ord k => (k -> String) -> (a -> String) -> M.Map k a -> String
showMap showKey showValue m =
  concat $ zipWith (++)
    (map showKey . M.keys $ m :: [String])
    (map (showValue . (M.!) m) . M.keys $ m :: [String])
