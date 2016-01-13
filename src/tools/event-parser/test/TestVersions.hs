{-
This test parses sample event logs from each major GHC version and compares
the pretty printed output with reference output.

When tests fail, use a diff tool to compare the output of "ghc-events show" with
the reference file.  Resolve the differences in either the library code or the
reference file, and 'darcs record' the changes.

Steps to produce event log and reference output:
    $ ghc --make queens.hs -o queens-ghc-$VERSION -threaded -eventlog
    $ queens-ghc-$VERSION 8 +RTS -N4 -ls
    $ ghc-events show queens-ghc-$VERSION.eventlog > queens-ghc-$VERSION.eventlog.reference

Where queens.hs is http://darcs.haskell.org/nofib/parallel/queens/Main.hs
-}

import GHC.RTS.Events
import System.Exit

files :: [FilePath]
files = map ("test/"++)
    [ "queens-ghc-6.12.1.eventlog"
    , "queens-ghc-7.0.2.eventlog"
    , "mandelbrot-mmc-2011-06-14.eventlog" 
    , "mdlLogMPI1.eventlog"
    , "pre77stop.eventlog", "782stop.eventlog", "783stop.eventlog" ]

-- returns True on success
testFile :: FilePath -> IO Bool
testFile f = do
    e <- readEventLogFromFile f
    let oops s = putStrLn (f ++ ": failure" ++ s) >> return False

    case e of
        Left m -> oops m

        Right newlogdata -> do
            oldlog <- readFile (f ++ ".reference")
            let newlog = ppEventLog newlogdata ++ "\n" in
                if oldlog == newlog
                    then putStrLn (f ++ ": success") >> return True
                    else do putStrLn $ diffLines oldlog newlog
                            oops "pretty print output does not match"

main = do
    successes <- mapM testFile files
    if and successes
        then return ()
        else exitFailure

--
-- Code to help print the differences between a working test and a failing test.
--

diffLines o n = diff 1 (lines o) (lines n)

diff :: Int -> [String] -> [String] -> String
diff _ [] [] = "Logs match"
diff l [] (n:ns) = "Extra lines in new log at line " ++ show l ++ ":\n" ++
    (unlines (n:ns))
diff l (o:os) [] = "Missing lines in new log at line " ++ show l ++ ":\n" ++
    (unlines (o:os))
diff l (o:os) (n:ns) = if (o == n)
                        then diff (l+1) os ns
                        else "Different lines at line " ++ show l ++ ":\n" ++
                            "Original: " ++ o ++ "\n" ++
                            "New:      " ++ n
