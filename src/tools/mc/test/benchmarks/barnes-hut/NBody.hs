--  Barnes-Hut n-body simulation
--
--  Author : Manuel M. T. Chakravarty & Gabriele Keller
--  Created: December 1997
--
--  Version $Revision: 1.2 $ from $Date: 1999/06/21 03:03:59 $
--
--  Copyright (c) [1997..1999] Chakravarty/Keller
--
--  THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU PUBLIC LICENCE
--  NO WARRANTY WHATSOEVER IS PROVIDED
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  N-body simulation using the Barnes-Hut algorithm.  The code is not
--  particularly optimized.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 1.4, but trying to be close to Nesl
--
--  tested with GHC 3.02, GHC 4.02, Hugs 98 beta
--
--  * Compilation with GHC:
--
--      ghc -fglasgow-exts -o nbody NBody.hs
--
--    with `-O2', you also need `-H10M'
--
--  * For other systems, the debugging code relying on special GHC/Hugs
--    features must be commented out - these lines are marked by `!GHC'.
--
--- TODO ----------------------------------------------------------------------
--
--

module Main (main)
where

import List
import Monad
import IO
import System    (ExitCode(..), getArgs, exitWith)
--import Directory (doesFileExist)
--not supported by Hugs

import IOExts    (trace)	-- !GHC


-- constants
-- ---------

gravConst :: Float
gravConst  = 6.670e-11


-- general parameters
-- ------------------

-- precision (a cell is *far away* if `l/d < theta')
--
theta :: Float
theta  = 0.8

-- Epsilon value
--
epsilon :: Float
epsilon  = 1.0e-20

-- Suffix for particles and states files.
--
particlesSuffix, statesSuffix :: String
particlesSuffix                = ".particles"
statesSuffix		       = ".states"


-- misc. aux. functions
-- --------------------

-- Merge two lists according to the given flag vector (`True' flags take
-- elements from the first list).
--
-- PRECONDITION: The number of `True' elements is equal to the sum of the
--		 length of the two combined lists.
--
combine                                          :: [Bool] -> [a] -> [a] -> [a]
combine []     _  _               = []
combine (f:fs) xs ys | f          = head xs : combine fs (tail xs) ys
		     | otherwise  = head ys : combine fs xs        (tail ys)

-- Checks whether the given file exists; this function is usually provided by
-- the library `Directory'; it is defined here as Hugs does not support the
-- `Directory' module
--
doesFileExist       :: FilePath -> IO Bool
doesFileExist fname  = (do
		          hdl <- openFile fname ReadMode
			  hClose hdl
			  return True
		       ) `catch` \_ -> return False


-- data types and functions for the physical parameter
-- ---------------------------------------------------

type Vector   = (Float, Float)
type Point    = Vector
type Veloc    = Vector
type Accel    = Vector
type Area     = (Point, Point)            -- incl. the 1st and excl. the 2nd
data MassPnt  = MassPnt Float Point       -- mass & location
	      deriving (Show)
data Particle = Particle MassPnt Veloc    -- mass point & velocity
	      deriving (Show)

-- cut an area into four equally sized pieces
--
--
-- Note: The layout is a3 a4
--		       a1 a2
--
cut                      :: Area -> (Area, Area, Area, Area)
cut ((x1, y1), (x2, y2))  = (a1, a2, a3, a4)
			    where
			      xm = x1 + (x2 - x1) / 2
			      ym = y1 + (y2 - y1) / 2
			      a1 = ((x1, y1), (xm, ym))
			      a2 = ((xm, y1), (x2, ym))
			      a3 = ((x1, ym), (xm, y2))
			      a4 = ((xm, ym), (x2, y2))

-- Superimpose a list of vectors
--
superimp    :: [Vector] -> Vector
superimp fs  = let
	         (fxs, fys) = unzip fs
	       in
	         (sum fxs, sum fys)

-- Acceleration modulo the gravity constant imposed *by* the first *on* the
-- second.  No acceleration if the position of both points is considered to be
-- equal, i.e., is smaller than `epsilon'.
--
-- * The second component of the result is `1' if the particles are not to
--   close for an interaction.
--
accel			                 :: MassPnt -> MassPnt -> (Accel, Int)
accel (MassPnt m (x1, y1)) 
      (MassPnt _ (x2, y2)) | r < epsilon  = ((0.0, 0.0), 0)
			   | otherwise    = ((aabs * dx / r, aabs * dy / r), 1)
					     where
					       rsqr = (dx * dx) + (dy * dy)
					       r    = sqrt rsqr
					       dx   = x1 - x2
					       dy   = y1 - y2
					       aabs = m / rsqr

--  Calculates the centroid of a list of mass points. 
--
centroid :: [MassPnt] -> MassPnt
centroid mps  = let
		  m          = sum [m | MassPnt m _ <- mps]
		  (wxs, wys) = unzip [(m * x, m * y) | MassPnt m (x, y) <- mps]
		in
		  MassPnt m (sum wxs / m, sum wys / m) 

-- Calculate the minimal bounding box for the given particle set.
--
-- PRECONDITION: The particles must contain at least one element.
--
-- Note: The bounding box is extended by `epsilon' as t represents an open
--	 interval. 
--
boundingBox     :: [MassPnt] -> Area
boundingBox mps  = let
		     xys      = [xy | MassPnt _ xy <- mps]
		     (xs, ys) = unzip xys
		   in
		     ((minimum xs - epsilon, minimum ys - epsilon), 
		      (maximum xs + epsilon, maximum ys + epsilon))

-- Returns the maximal side length of an area.
--
maxSideLen                      :: Area -> Float
maxSideLen ((x1, y1), (x2, y2))  = if dx > dy then dx else dy
				   where
				     dx = abs (x1 - x2)
				     dy = abs (y1 - y2)

-- Calculate the maximal relative error of the positions of two particle lists
-- (the first argument provides the reference values).
--
-- PRECONDITION: Both lists are of equal length.
--
maxErr        :: [Particle] -> [Particle] -> Float
maxErr rvs vs  = maximum (zipWith compare rvs vs)
  where
    compare (Particle (MassPnt _ (x1, y1)) _)
            (Particle (MassPnt _ (x2, y2)) _) = dr / r
						where
						  dx = abs (x1 - x2)
						  dy = abs (y1 - y2)
						  dr = sqrt (dx * dx + dy * dy)
						  r  = sqrt (x1 * x1 + y1 * y1)

-- tree data type
-- --------------

data Tree a = Node a [Tree a]
	    deriving (Show)

-- A tree consists merely a leaf node, if it has no children
--
isLeaf                 :: Tree a -> Bool
isLeaf (Node _ childs)  = null childs


-- Barnes-Hut algorithm
-- --------------------

-- Build a quadtree from a particle list.
--
-- The given mass points (which have to lie in the given area), are stored
-- in a quadtree where one leaf contains at most `particlesPerCell' particles.
-- 
-- The resulting tree consists of two kinds of nodes: leaf nodes and inner
-- nodes.  Leaf node store a `real' mass point in the first argument of `Node'
-- and inner nodes store the centroid of the subtree in this position.  Leaf
-- nodes are identified by an empty list of subtrees.
--
bhTree      :: Area -> [MassPnt] -> Tree MassPnt
bhTree a ps  = if length ps == 1
	       then Node (head ps) []			-- leaf
	       else let					-- inner node
		      (a1, a2, a3, a4) = cut a
		      (_, (xm, ym))    = a1
		      flags            = [ (x < xm, y < ym) 
					 | MassPnt _ (x, y) <- ps]

		      ps1              = [p | (p, (fx, fy)) <- zip ps flags,
					      fx && fy]
		      ps2              = [p | (p, (fx, fy)) <- zip ps flags,
					      not fx && fy]
		      ps3              = [p | (p, (fx, fy)) <- zip ps flags,
					      fx && not fy]
		      ps4              = [p | (p, (fx, fy)) <- zip ps flags,
					      not fx && not fy]
-- naive:
--		     ps1              = [p | p <- ps, p `within` a1]
--		     ps2              = [p | p <- ps, p `within` a2]
--		     ps3              = [p | p <- ps, p `within` a3]
--		     ps4              = [p | p <- ps, p `within` a4]

		      childs   = [bhTree a ps
				 | (a, ps) <- zip [a1, a2, a3, a4] 
					  	  [ps1, ps2, ps3, ps4],
				   (not . null) ps]
		      cd = centroid [mp | Node mp _ <- childs]
		    in
		      Node cd childs

-- Calculates whether the two mass points are far enough apart to allow an
-- approximation of the gravity force between them (given the cell size
-- provided as the first argument). 
--
isFar :: Float -> MassPnt -> MassPnt -> Bool
isFar l (MassPnt _ (x1, y1)) (MassPnt _ (x2, y2)) = 
  if r < epsilon then False else l / r < theta
  where
    r  = sqrt ((dx * dx) + (dy * dy))
    dx = x2 - x1
    dy = y2 - y1

-- Calculates the acceleration on a set of particles according to the given
-- Barnes-Hut tree (whose bounding box has a maximum side length as given in
-- the second argument).
--
-- * In addition to the accelerations, the number of direct and far-field
--   interactions is computed.
--
accels :: Tree MassPnt -> Float -> [MassPnt] -> ([Accel], Int, Int)
accels _             _   []   = ([], 0, 0)
accels (Node crd []) len mps  = 
  let
    (acs, noAcs) = unzip [accel crd mp | mp <- mps]
  in
    (acs, sum noAcs, 0)
accels (Node crd ts) len mps  = 
  let 
    direct     = [isFar len crd mp | mp <- mps]
    farMps     = [mp | (mp, isFar) <- zip mps direct, isFar]
    closeMps   = [mp | (mp, isFar) <- zip mps direct, not isFar]
    (farAcs,
     noFarAcs) = unzip [accel crd mp | mp <- farMps]
    (closeAcss,
     directNos,
     farNos)   = unzip3 [accels t (len / 2) closeMps   -- `closeMps' free!!
		        | t <- ts]
    closeAcs   = [superimp oneMpAcs | oneMpAcs <- transpose closeAcss]
  in
    (combine direct farAcs closeAcs,	-- combined accelerations
     sum directNos,			-- direct interactions
     sum farNos + sum noFarAcs)		-- recursive and current far interact.

-- Calculate the new velocity of each particle after the time `dt' under the
-- corresponding accelaration in `acs'.
--
applyAccels :: Float -> [Accel] -> [Particle] -> [Particle]
applyAccels dt acs ps  = [ Particle mp (vx + ax * dt, vy + ay * dt)
			 | (Particle mp (vx, vy), (ax, ay)) <- zip ps acs]

-- Given a set of particles, move them according to their velocities during a
-- `dt' time interval.
--
move       :: Float -> [Particle] -> [Particle]
move dt ps  = [ Particle (MassPnt m (x + vx * dt, y + vy * dt)) v
	      | Particle (MassPnt m (x, y)) v@(vx, vy) <- ps]

-- Execute a single iteration (tree construction, accelaration calculation, and
-- update of the velocities and positions).  The first argument determines the
-- time interval. 
--
-- PRECONDITION: The particles must contain at least one element.
--
-- * In addition to the new particles, the number of direct and far-field
--   interactions is computed.
--
oneStep            :: Mode -> Float -> [Particle] -> ([Particle], Int, Int)
oneStep mode dt ps  = 
  let
    mps         = [mp | Particle mp _ <- ps]
    box         = boundingBox mps
    len         = maxSideLen box
    t           = bhTree box mps
    (preAcs, 
     directNos, 
     farNos)    = accels t len mps
    acs		= [ (gravConst * ax, gravConst * ay) 
		  | (ax, ay) <- preAcs]
    ps'		= applyAccels dt acs ps
  in
    mayTrace mode t acs $					-- !GHC
    (move dt ps', directNos, farNos)
  where								-- !GHC
    mayTrace Debug t acs = trace ("===== BH tree =====\n"	-- !GHC
				  ++ show t ++			-- !GHC
				  "\n== Accelerations ==\n"	-- !GHC
				  ++ show acs ++		-- !GHC
				  "\n=====================")	-- !GHC
    mayTrace _     _ _   = id					-- !GHC

-- One step of the n-body simulation using the naive O(n^2) algorithm.  The
-- first argument determines the time interval.
--
-- * The number of interactions is returned in the second component.
--
naiveStep       :: Float -> [Particle] -> ([Particle], Int)
naiveStep dt ps  = let
		     (acs, noAcs) = unzip [ allAccels mp ps 
					  | Particle mp v <- ps]
		     ps'          = applyAccels dt acs ps
		   in
		     (move dt ps', sum noAcs)
		   where
		     allAccels       :: MassPnt -> [Particle] -> (Accel, Int)
		     allAccels mp ps  = 
		       let
			 (acs, noAcs) = unzip [ accel mp' mp 
					      | Particle mp' _ <- ps]
			 (axs, ays)   = unzip acs
		       in
			 ((gravConst * sum axs, gravConst * sum ays), 
			  sum noAcs)


-- Wrapper
-- -------

-- Read a particle sequence from the file with the given name.
--
-- Format: 
--  * a particle a line
--  * <mass> <x>,<y> <vx>,<vy>
--
readParticles       :: FilePath -> IO [Particle]
readParticles fname  = 
  do
    f <- openFile fname ReadMode
      `catch` \_ -> halt ("Couldn't open `" ++ fname ++ "'!")
    s <- hGetContents f
    return (toParticles s)
  where
    toParticles   :: String -> [Particle]
    toParticles s  = [toParticle l pstr | (l, pstr) <- zip [1..] (lines s)]

    toParticle     :: Int -> String -> Particle
    toParticle l s  = 
      case words s of
	[mstr, xystr, vstr] -> let 
			         m = read mstr :: Float
			       in
			       if abs m < epsilon 
			       then
			         error ("nbody: Error in particle file \
				        \(mass smaller than " 
					++ show epsilon
				        ++ "), line " 
				        ++ show l ++ "!\n")
			       else
				 Particle (MassPnt m (toVector l xystr)) 
					  (toVector l vstr)
	_                   -> error ("nbody: Error in particle file, line " 
				      ++ show l ++ "!\n")

    toVector     :: Int -> String -> Vector
    toVector l s  = case break (== ',') s of
		      (xstr, ',':ystr) -> (read xstr :: Float, 
					   read ystr :: Float)
		      _		       -> 
		        error ("nbody: Error in particle file, line " 
			       ++ show l ++ "!\n")

-- Dump a particle sequence to a file with the given name.
--
-- Format: 
--  * a particle a line
--  * <mass> <x>,<y> <vx>,<vy>
--
dumpParticles          :: FilePath -> [Particle] -> IO ()
dumpParticles fname ps  = 
  do
    f <- openFile fname WriteMode
    mapM_ (dumpParticle f) ps
    hClose f
  where
    dumpParticle :: Handle -> Particle -> IO ()
    dumpParticle f (Particle (MassPnt m (x, y)) (vx, vy)) =
      do
	hPutShow f m
	hPutChar f ' '
	hPutShow f x
	hPutChar f ','
	hPutShow f y
	hPutChar f ' '
	hPutShow f vx
	hPutChar f ','
	hPutShow f vy
	hPutChar f '\n'
    hPutShow   :: Show a => Handle -> a -> IO ()
    hPutShow f  = hPutStr f . show

-- Dump a state (particle positions) to the given file.
--
-- Format: 
--  * a particle a line
--  * x and y coordinate separated by a space
--
dumpState      :: Handle -> [Particle] -> IO ()
dumpState f ps  = 
  mapM_ (dumpState f) ps
  where
    dumpState :: Handle -> Particle -> IO ()
    dumpState f (Particle (MassPnt _ (x, y)) _) =
      do
	hPutShow f x
	hPutChar f ' '
	hPutShow f y
	hPutChar f '\n'
    hPutShow   :: Show a => Handle -> a -> IO ()
    hPutShow f  = hPutStr f . show

-- Operation modes
--
data Mode = BarnesHut	-- use Barnes-Hut algorithm
	  | Naive	-- use naive O(n^2) algorithm
	  | ErrorCheck  -- use Barnes-Hut and calculate the error wrt to naive
	  | Debug	-- trace tree construction and acceleration values
	  deriving (Eq)

-- Analyze the command line.
--
-- Returns (1) the filename (base) of the particles file,
--	   (2) the number of iterations,
--	   (3) the time interval per iteration, 
--	   (4) the mode of the simulation, and
--	   (5) whether the number of interactions should be counted.
--
analyzeArgs :: IO (String, Int, Float, Mode, Bool)
analyzeArgs  = 
  do
    args <- getArgs
    let (args', mode)   = if ((not . null) args) && head args == "-n"
			  then
			    (tail args, Naive)
			  else if ((not . null) args) && head args == "-e"
			  then
			    (tail args, ErrorCheck)
			  else if ((not . null) args) && head args == "-d"
			  then
			    (tail args, Debug)
			  else
			    (args, BarnesHut) 
        (args'', count) = if ((not . null) args) && head args == "-i"
			  then
			    (tail args, True)
			  else 
			    (args, False)
    case args'' of
      [fbase, noIter, ts] -> do
			       let fname = fbase ++ particlesSuffix
			           n     = read noIter :: Int
				   dt    = read ts     :: Float
			       exists <- doesFileExist fname
			       (if not exists
				then
				  haltWithUsage ("`" ++ fname 
						 ++ "' does not exist!")
				else if not (n > 0) 
				then
				  haltWithUsage "Number of iterations must be \
						\greater than zero!"
				else if not (dt > epsilon) 
				then
				  haltWithUsage ("Interval must be greater \
						 \than " ++ show epsilon 
						 ++ "!")
				else
				  return (fbase, n, dt, mode, count))
      _                   -> haltWithUsage "Wrong number of command line \
					   \arguments!"

-- Stop execution after printing the given error message. 
--
halt     :: String -> IO a
halt err  = 
  do
    putStr ("nbody: " ++ err ++ "\n")
    exitWith (ExitFailure 1)

-- Stop execution after printing the given error message and the program usage
-- information. 
--
haltWithUsage     :: String -> IO a
haltWithUsage err  = 
  do
    putStr usage
    exitWith (ExitFailure 1)
  where
    usage = "nbody: " ++ err ++ "\n\
	    \usage: nbody [-d|-e|-n] [-i] <fname> <N> <dt>\n\n\
	    \  -d       Debug mode (trace tree construction and accels)\n\
	    \  -e       Barnes-Hut, but use naive for error check\n\
	    \  -n       naive, O(n^2) algorithm\n\
	    \  -i       count interactions\n\
	    \  <fname>  basename of particle file\n\
	    \  <N>      number of interations (natural number > 0)\n\
	    \  <dt>     time interval per iteration (float > " 
	    ++ show epsilon ++ ")\n\n\
	    \ Particle input file <fname>" ++ particlesSuffix ++ " and\n\
	    \ iteration state output <fname>" ++ statesSuffix ++ "\n"

-- Read command line and execute the simulation.
--
-- Output format:
--  * output in <fname>.`statesSuffix'
--  * <N> + 1 states
--  * each state is introduced by a line of the form #<step> where <step> is
--    the number of the iteration (first dump is #0 with the original
--    positions) 
--  * the particle positions are formatted according to the docu for
--    `dumpParticles' 
--
main :: IO ()
main  = do
	  -- analyze command line
	  --
	  (fname, n, dt, mode, count) <- analyzeArgs
	  --
	  -- read particles and check that the list isn't empty
	  --
	  ps <- readParticles (fname ++ particlesSuffix)
	  when (null ps) 
	       (halt "Empty particle file!")
	  --
	  -- open the `.states' file
	  --
	  outFile <- openFile (fname ++ statesSuffix) WriteMode
	    `catch` \_ -> halt ("Couldn't open `" ++ fname ++ statesSuffix 
				++ "'!")
	  --
	  -- dump the initial state into the `.states' file
	  --
	  hPutStr outFile "#0\n"
	  dumpState outFile ps
	  --
	  -- do the simulation
	  --
	  let
	    iter :: ([Particle], Float) -> Int -> IO ([Particle], Float)
	    iter (ps, err) step  = 
	      do
		let (bhPs, 
		     dNos, 
		     fNos)    = oneStep mode dt ps
		    (naivePs,
		     nNos)    = naiveStep dt ps		-- nNos not used yet
		    ps'       = if mode == Naive then naivePs else bhPs
		hPutStr outFile ("#" ++ show step ++ "\n")
		dumpState outFile ps'
		when count $
		  hPutStr outFile ("%" ++ show dNos ++ " + " 
				   ++ show fNos ++ "\n")
		(if mode == ErrorCheck
		 then
		   return (ps', max err (maxErr naivePs bhPs))
		 else
		   return (ps', err))
	  (_, err) <- foldM iter (ps, 0.0) [1..n]
	  when (mode == ErrorCheck)
	       (putStr ("Maximal relative error: " ++ show err ++ "\n"))
	  --
	  -- clean up and terminate
	  --
	  hClose outFile
	  exitWith ExitSuccess
