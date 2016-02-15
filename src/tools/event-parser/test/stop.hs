-- This test program triggers different thread stop encodings in
-- eventlogs, depending on GHC version (black hole, mvar read, mvar)

module Main where

import Control.Concurrent
import Debug.Trace
import GHC.Conc

main = do 
  putStrLn "suggest to run with +RTS -lsu-g-p -K80m -k10m -H200m -C1s"

  -- define some time-consuming computation
  let stuff = ack 3 10
  -- create MVars to block on
  v1 <- newMVar "full"
  v2 <- newEmptyMVar
  -- create a thread which blackholes something, and re-fills the MVar
  traceEventIO "forking child thread"
  forkIO (do traceEventIO "child"
             putStrLn ("child thread sez " ++ show stuff)
             traceEventIO "filling full MVar"
             putMVar v1 "filled full var"
             yield
             traceEventIO "filling empty MVar"
             putMVar v2 "filled empty var"
             yield
             traceEventIO "child finished"
         )
  yield
  putStrLn ("and the main thread sez " ++ show stuff)
  traceEventIO "emptying full MVar"
  s1 <- takeMVar v1
  putStrLn ("from MVar: " ++ s1)
  traceEventIO "reading empty MVar"
  s2 <- readMVar v2
  putStrLn ("from MVar: " ++ s2)

ack :: Integer -> Integer -> Integer
ack 0 m = m+1
ack n 0 = ack (n-1) 1
ack n m = ack (n-1) (ack n (m-1))
