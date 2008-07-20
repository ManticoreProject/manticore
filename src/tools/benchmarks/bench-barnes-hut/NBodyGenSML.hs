--  Particle generator for 2D n-body simulations
--
--  Author : Manuel M. T. Chakravarty & Gabriele Keller
--  Created: January 1998
--
--  Version $Revision: 1.3 $ from $Date: 1999/10/19 09:16:49 $
--
--  Copyright (c) [1997..1999] Chakravarty/Keller
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This program generates randomized particle clusters for 2D n-body
--  simulations.  The code is rather naive at the moment, as filtering out of
--  particles that are too close to each other is rather simple.  So, please
--  don't expect any performance wonders.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Properties of the generated particle set:
--
--  * The distance between each pair of particles is greater than `epsilon'.
--
--  * The area where particles occur is centered at the origin of the
--    coordinate system.
--
--  * The plummer distribution uses the same parameters as Jan Prins et al.'s
--    `plummer.F90' module.  Jan writes, ``The plummer distribution places n
--    bodies in d-space s.t.
--
--    - each body has mass 1/n
--
--    - each body i is placed at a random position distance s_i from the
--      origin, where
--
--        s_i = rsc * r_i
--        rsc = (3 * pi) / 16
--        r_i = sqrt( .999 * w^(-2/3) - 1)
--        w is a uniform random number on [0,1)
--
--    - each body i is given a random initial velocity whose magnitude
--      is u_i, where
--
--        u_i  = vsc * v_i
--        vsc  = 1 / sqrt(rsc)
--        v_i  = x * sqrt(2) / (1 + r_i^2)^(1/4)
--        x is a uniform random number on [0,1) such that
--               y <= x^2 * (1-x^2)^(7/2)
--        y is a uniform random number on [0, 0.1)
--
--    These positions and velocities are then translated so that the center
--    of mass and center of velocity (total momentum) are 0 in d-space.''
--
--- TODO ----------------------------------------------------------------------
--
--  * check for !!! in `plummerParticles'
--
--  * 3D
--
--  * there is still a distribution missing
--
--  * command line parsing is a mess
--

module Main
where

import Monad   (liftM)
import Numeric (showFloat, showFFloat)

import List   (nubBy)
import IO
import System (ExitCode(..), getArgs, exitWith)
import Random (Random, RandomGen, getStdGen, randoms, randomRs)


-- general parameters
-- ------------------

-- Epsilon value
--
epsilon :: Float
epsilon  = 1.0e-10

-- Suffix for particles files.
--
particlesSuffix :: String
particlesSuffix  = ".sml"


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


-- Test whether the Manhattan distance between two points is smaller than
-- `epsilon'. 
--
epsilonEqual                    :: Point -> Point -> Bool
epsilonEqual  (x1, y1) (x2, y2)  = abs (x1 - x2) + abs (y1 - y2) < epsilon

-- Drop all mass points that are too close to another.
--
nubMassPnts :: [MassPnt] -> [MassPnt]
nubMassPnts  = nubBy (\(MassPnt _ p1) (MassPnt _ p2) -> epsilonEqual p1 p2)

-- Same for particles.
--
nubParticles :: [Particle] -> [Particle]
nubParticles  = nubBy (\(Particle (MassPnt _ p1) _) 
		        (Particle (MassPnt _ p2) _)-> epsilonEqual p1 p2)

--  Calculates the centroid of a list of mass points. 
--
centroid     :: [MassPnt] -> MassPnt
centroid mps  = let
		  m          = sum [m | MassPnt m _ <- mps]
		  (wxs, wys) = unzip [(m * x, m * y) | MassPnt m (x, y) <- mps]
		in
		  MassPnt m (sum wxs / m, sum wys / m) 

--  Calculates the total momentum.
--
totalMomentum    :: [Particle] -> (Float, Point)
totalMomentum ps  = 
  let
    m          = sum [m | (Particle (MassPnt m _) _) <- ps]
    (wxs, wys) = unzip [(m * x, m * y) | (Particle (MassPnt m _) (x, y)) <- ps]
  in
    (m, (sum wxs / m, sum wys / m))

-- translate a particle
--
translate :: Point -> Particle -> Particle
translate (dx, dy) (Particle (MassPnt m (x, y)) vxy) =
  Particle (MassPnt m (x + dx, y + dy)) vxy

-- translate the velocity of particle
--
translateVel :: Point -> Particle -> Particle
translateVel (dvx, dvy) (Particle mp (vx, vy)) =
  Particle mp (vx + dvx, vy + dvy)


-- particle generation
-- -------------------

randomTo, randomFrom :: Integer
randomTo    = 2^30
randomFrom  = - randomTo

randomRIOs       :: Random a => (a, a) -> IO [a]
randomRIOs range  = liftM (randomRs range) getStdGen 

randomIOs :: Random a => IO [a]
randomIOs  = liftM randoms getStdGen 

--  generate a stream of random numbers in [0, 1)
--
randomFloatIO :: IO [Float]
randomFloatIO  = randomIOs

-- generate an infinite list of random mass points located with a homogeneous
-- distribution around the origin within the given bounds
--
randomMassPntsIO       :: Float -> Float -> IO [MassPnt]
randomMassPntsIO dx dy  = do
			    rs <- randomRIOs (randomFrom, randomTo)
			    return (massPnts rs)
			  where
			    to    = fromIntegral randomTo
			    from  = fromIntegral randomFrom
			    xmin  = - (dx / 2.0)
			    ymin  = - (dy / 2.0)
			    xfrac = (to - from) / dx
			    yfrac = (to - from) / dy

			    massPnts               :: [Integer] -> [MassPnt]
			    massPnts (xb:yb:mb:rs)  = 
			      MassPnt m (x, y) : massPnts rs
			      where
				m = (fromInteger . abs) mb + epsilon
				x = xmin + (fromInteger xb) / xfrac
				y = ymin + (fromInteger yb) / yfrac

-- The mass of the generated particle cloud is standardized to about 
-- 5.0e7 g/m^2.  The mass of individual particles may deviate by a factor of
-- ten from the average.
--
smoothMass           :: Float -> Float -> [MassPnt] -> [MassPnt]
smoothMass dx dy mps  = let
			  avmass = 5.0e7
			  area   = dx * dy
			  middle = avmass * area / fromIntegral (length mps)
			  range  = fromIntegral (randomTo - randomFrom)
			  factor = (middle * 10 - middle / 10) / range

			  adjust (MassPnt m xy) = 
			    MassPnt (middle + factor * m) xy 
			in
			  map adjust mps

-- Given the number of particles to generate and the horizontal and vertical
-- extensions of the area where the generated particles should occur, generate
-- a particle set according to a function specific strategy.
--
asymTwinParticles, 
  sphereParticles, 
  plummerParticles, 
  homParticles    :: Int -> Float -> Float -> IO ([Particle])

asymTwinParticles n dx dy = error "asymTwinPrticles not implemented yet\n"

sphereParticles n dx dy = 
  do
    let rad = dx `min` dy
    mps <- randomMassPntsIO dx dy
    return ((  map (\mp -> Particle mp (0.0, 0.0))
	     . smoothMass dx dy
	     . head 
	     . filter ((== n) . length) 
	     . map fst 
	     . iterate refine
	    )  ([], filter (inside rad) mps)
	   )
  where
    --
    -- move suitable mass points from the second list to the first (i.e., those
    -- not conflicting with points that are already in the first list)
    --
    refine :: ([MassPnt], [MassPnt]) -> ([MassPnt], [MassPnt])
    refine (ds, rs) = let
		        (ns, rs') = splitAt (n - length ds) rs
		      in
		        (nubMassPnts (ds ++ ns), rs')

    -- check whether inside the given radius
    --
    inside                          :: Float -> MassPnt -> Bool
    inside rad (MassPnt _ (dx, dy))  = dx * dx + dy * dy <= rad * rad

plummerParticles n _ _ =
  do
    rs <- randomFloatIO
    return ((   normalize
	      . head 
	      . filter ((== n) . length) 
	      . map fst 
	      . iterate refine
	     ) ([], particles rs)
	    )
  where
    particles (w:preY:rs') = let
			       s_i = rsc * r_i
			       rsc = (3 * pi) / 16
			       r_i = sqrt' ((0.999 * w)`power`(-2/3) - 1)
			       --
			       u_i = vsc * v_i
			       vsc = 1 / sqrt rsc
			       v_i = (x * sqrt 2) / (1 + r_i^2)**(1/4)
			       --
			       (pos, rs''' ) = rndVec s_i rs''
			       (vel, rs'''') = rndVec u_i rs'''
			     in
			     Particle (MassPnt m pos) vel : particles rs''''
			     where
			       y	 = preY / 101
						  -- !!!should be 10, but then
						  -- !!!findX gets problems
			       (x, rs'') = findX y rs'
			       --
			       m         = 1 / fromIntegral n
			       --
			       x`power`y | x == 0.0  = 0.0
					 | otherwise = x**y
			       sqrt' x   | x < 0     = 0
					 | otherwise = sqrt x

    findX :: Float -> [Float] -> (Float, [Float])
    findX y (x:rs) | y <= x^2 * (1 - x^2)**(7/2) = (x, rs)
		   | otherwise			  = findX y rs

    rndVec len (x:y:rs) = let r = len / sqrt (x^2 + y^2)
			  in
			  ((r * x, r * y), rs)

    -- move suitable mass points from the second list to the first (i.e., those
    -- not conflicting with points that are already in the first list)
    --
    refine :: ([Particle], [Particle]) -> ([Particle], [Particle])
    refine (ds, rs) = let
		        (ns, rs') = splitAt (n - length ds) rs
		      in
		        (nubParticles (ds ++ ns), rs')

    -- translate positions and velocities such that they are at the origin
    --
    normalize    :: [Particle] -> [Particle]
    normalize ps  = 
      let MassPnt _ (dx, dy) = centroid [mp | Particle mp _ <- ps]
	  (_, (dvx, dvy))    = totalMomentum ps
      in
      (map (translateVel (-dvx, -dvy)) . map (translate (-dx, -dy))) ps


homParticles n dx dy = 
  do
    mps <- randomMassPntsIO dx dy
    return ((  map (\mp -> Particle mp (0.0, 0.0))
	     . smoothMass dx dy
	     . head 
	     . filter ((== n) . length) 
	     . map fst 
	     . iterate refine
	    )  ([], mps)
	   )
  where
    --
    -- move suitable mass points from the second list to the first (i.e., those
    -- not conflicting with points that are already in the first list)
    --
    refine :: ([MassPnt], [MassPnt]) -> ([MassPnt], [MassPnt])
    refine (ds, rs) = let
		        (ns, rs') = splitAt (n - length ds) rs
		      in
		        (nubMassPnts (ds ++ ns), rs')


-- Wrapper
-- -------

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
    hPutChar f '\n'
    hPutStr f "structure Particles = struct"
    hPutChar f '\n'
    hPutStr f "val particles = ["
    dumpParticles f ps
--    mapM_ (dumpParticle f) ps
    hPutStr f "]"
    hPutChar f '\n'
    hPutStr f "end"
    hPutChar f '\n'
    hClose f
  where
    rep '-' = '~'
    rep x = x    
    showf y = map rep ((showFFloat Nothing y) "")
    dumpParticles :: Handle -> [Particle] -> IO ()
    dumpParticles f [] = return ()
    dumpParticles f [p] = 
      do
        dumpParticle f p
	hPutChar f '\n'
    dumpParticles f (p : ps) =
      do
        dumpParticle f p
	hPutChar f ','
	hPutChar f '\n'
        dumpParticles f ps
    dumpParticle f (Particle (MassPnt m (x, y)) (vx, vy)) =
      do
        hPutChar f '('
	hPutF f m
	hPutChar f ','
	hPutF f x
	hPutChar f ','
	hPutF f y
	hPutChar f ','
	hPutF f vx
	hPutChar f ','
	hPutF f vy
	hPutChar f ')'
    hPutShow   :: Show a => Handle -> a -> IO ()
    hPutShow f  = hPutStr f . show
    hPutF f  = hPutStr f . showf

-- Analyze the command line.
--
-- Returns the function that is used to generate the particles and the file
-- name of the particle file.
--
analyzeArgs :: IO ((IO ([Particle]), FilePath))
analyzeArgs  = 
  do
    args <- getArgs
    let (particles, 
	 args',
	 fixedDXY ) = if ((not . null) args) && head args == "-a"
		      then
			(asymTwinParticles, tail args, False)
		      else if ((not . null) args) && head args == "-p"
		      then
			(plummerParticles, tail args, True)
		      else if ((not . null) args) && head args == "-s"
		      then
			(sphereParticles, tail args, False)
		      else 
			(homParticles, args, False)
    case (args', fixedDXY) of
      ([no, dx, dy, fbase], False) -> 
        do
	  let fname = fbase ++ particlesSuffix
	      n     = read no :: Int
	      dxVal = read dx :: Float
	      dyVal = read dy :: Float
	  (if not (n > 0) 
	   then
	     haltWithUsage "Number of particles must be \
			   \greater than zero!"
	   else if not (dxVal > epsilon 
			&& dyVal > epsilon)
	   then
	     haltWithUsage ("dx and dy must be greater \
			    \than " ++ show epsilon 
			    ++ "!")
	   else
	     return (particles n dxVal dyVal, fname))
      ([no, fbase], True) -> 
        do
	  let fname = fbase ++ particlesSuffix
	      n     = read no :: Int
	  (if not (n > 0) 
	   then
	     haltWithUsage "Number of particles must be \
			   \greater than zero!"
	   else
	     return (particles n undefined undefined, fname))
      _ -> haltWithUsage "Wrong number of command line arguments!"

-- Stop execution after printing the given error message and the program usage
-- information. 
--
haltWithUsage     :: String -> IO a
haltWithUsage err  = 
  do
    putStr usage
    exitWith (ExitFailure 1)
  where
    usage = "nbodygen: " ++ err ++ "\n\
	    \usage: nbodygen [-a|-s] <N> <dx> <dy> <fname>\n\
	    \usage: nbodygen -p      <N>           <fname>\n\n\
	    \  (default distribution is homogeneous)\n\
	    \  -a       asymmetric two cluster system \n\
	    \  -p       plummer distribution\n\
	    \  -s       sphere around the center\n\
	    \  <fname>  basename of particle file\n\
	    \  <N>      number of particles (natural number > 0)\n\
	    \  <dx>     horizontal extension of the used area (float > " 
	    ++ show epsilon ++ ")\n\
	    \  <dy>     vertical extension of the used area (float > " 
	    ++ show epsilon ++ ")\n\n\
	    \  Particle output file name: <fname>" ++ particlesSuffix ++ "\n"

-- Read command line and generate particle file.
--
main :: IO ()
main  = do
	  -- analyze command line, returning the particle generating function
	  --
	  (particles, fname) <- analyzeArgs
	  --
	  -- generate particles
	  --
	  ps <- particles
	  --
	  -- dump into the given file
	  --
	  dumpParticles fname ps
	  --
	  -- terminate
	  --
	  exitWith ExitSuccess
