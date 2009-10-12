(* barnes-hut.sml
 * 
 * Barnes Hut computation. This code is adapted from the Data-parallel
 * Haskell benchmark.
 * http://darcs.haskell.org/packages/ndp/examples/barnesHut/
 *) 

structure BarnesHut =
  struct

    fun ceilingLg (x :int) : int = Real.ceil (Math.log10 (Real.fromInt x) / Math.log10 2.0)

    structure Double = Real
    type double = Real.real
    datatype bounding_box = BOX of (double * double * double * double)
    datatype mass_point = MP of (double * double * double) (* xpos ypos mass *)
    datatype particle = PARTICLE of (mass_point * double * double) (* mass point and velocity *)
    datatype bh_tree 
      = BHT_QUAD of (double * double * double *     (* root mass point *)
		     bh_tree * bh_tree * bh_tree * bh_tree)
						    (* subtrees *)
      | BHT_LEAF of (double * double * double *     (* root mass point *)
		     mass_point list) 

    val epsilon : double = 0.05
    val eClose : double = 0.01
    val dt = 2.0

    fun xCoord (MP (x, _, _)) = x
    fun yCoord (MP (_, y, _)) = y

    fun applyAccel (PARTICLE (mp, vx, vy), (ax, ay)) = PARTICLE (mp, vx+ax * dt, vy+ay * dt)

    fun isClose (MP (x1, y1, m), x2, y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) < eClose

    fun accel (MP (x1,  y1, _), x2, y2, m)  =
	let
	    val dx   = x1 - x2 
	    val dy   = y1 - y2 
	    val rsqr = (dx * dx) + (dy * dy) 
	    val r    = Math.sqrt rsqr 
	in
	    if r < epsilon then
		(0.0, 0.0) 
	    else 
		let
		    val aabs = m / rsqr 
		in
		    (aabs * dx / r , aabs * dy / r) 
		end
	end

    (* the acceleration of a mass point after applying the force applied by surrounding
     * mass points
     *)
    fun calcAccel (mpt, bht) = 
	(case bht
	  of BHT_LEAF (x, y, m, _) =>
	     accel (mpt, x, y, m)
	   | BHT_QUAD (x, y, m, st1, st2, st3, st4) =>
	     if isClose (mpt, x, y) then
		 let
		     val ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) =
			 ( calcAccel (mpt, st1), 
			   calcAccel (mpt, st2), 
			   calcAccel (mpt, st3), 
			   calcAccel (mpt, st4) )
		 in
		     (x1 + x2 + x3 + x4, y1 + y2 + y3 + y4)
		 end
	     else
		 accel (mpt, x, y, m)
	(* end case *))

    fun calcCentroid (mpts : mass_point list) : mass_point = 
	let
	    fun circlePlus ((mx0,my0,m0), (mx1,my1,m1)) = (mx0+mx1, my0+my1, m0+m1)
	    val (sum_mx, sum_my, sum_m) = 
		List.foldl circlePlus (0.0, 0.0, 0.0) (List.map (fn (MP (x, y, m)) => (m*x, m*y, m)) mpts)
	in
	    MP (sum_mx / sum_m, sum_my / sum_m, sum_m)
	end

    fun inBox (BOX (llx, lly, rux, ruy)) (MP (px, py, _)) =
	(px > llx) andalso (px <= rux) andalso (py > lly) andalso (py <= ruy)

    (* returns the list of particles that are centered within the box and the number of those particles *)
    fun particlesInBox (box, particles) =
	let
	    fun filt box (p, (n, ps)) = if inBox box p then (n + 1, p :: ps) else (n, ps)
	in
	    List.foldl (filt box) (0, nil) particles
	end

    (* split mass points according to their locations in the quadrants *)
    fun buildTree (box, particles : mass_point list) : bh_tree =
	let
	    val nParticles = List.length particles
	    val maxDepth = ceilingLg nParticles
	    fun build (depth, BOX (llx, lly, rux, ruy), nParticles, particles) =
		(* note the condition for stopping based on the depth of our recursion tree. if we did not
		 * limit the depth, nontermination could occur when two or more particles lie on top of 
		 * each other. *)
		(* also note: our stopping condition means that, in the worst case, the depth of our tree is twice the
		 * depth of a perfectly balanced tree. *)
		if nParticles = 1 orelse depth > maxDepth then
		    let
			val MP (x, y, m) = calcCentroid particles
		    in
			BHT_LEAF (x, y, m, particles)
		    end
		else
		    let
			val MP (x, y, m) = calcCentroid particles
			val (midx,  midy) = ((llx + rux) / 2.0 , (lly + ruy) / 2.0) 
			val b1 = BOX (llx, lly, midx, midy)
			val b2 = BOX (llx,  midy, midx,  ruy)
			val b3 = BOX (midx, midy, rux,   ruy)
			val b4 = BOX (midx, lly,  rux,  midy)
			val (n1, p1) = particlesInBox (b1, particles)
			val (n2, p2) = particlesInBox (b2, particles)
			val (n3, p3) = particlesInBox (b3, particles)
			val (n4, p4) = particlesInBox (b4, particles)
			val depth' = depth + 1
			val (q1, q2, q3, q4) =
			    ( build (depth', b1, n1, p1),
			      build (depth', b2, n2, p2),
			      build (depth', b3, n3, p3),
			      build (depth', b4, n4, p4) )
		    in
			BHT_QUAD (x, y, m, q1, q2, q3, q4)
		    end
	in
	    build (0, box, nParticles, particles)
	end

    fun oneStep (pts : particle list) : particle list =
	(case pts
	  of nil => nil
	   | PARTICLE (MP (x0, y0, _), _, _) :: _ =>
	     let
		 val mspnts = List.map (fn (PARTICLE (mpnt, _, _)) => mpnt) pts
		 val llx = List.foldl Real.min x0 (List.map xCoord mspnts)
		 val lly = List.foldl Real.min y0 (List.map yCoord mspnts)
		 val rux = List.foldl Real.max x0 (List.map xCoord mspnts)
		 val ruy = List.foldl Real.max y0 (List.map yCoord mspnts)
		 val tree = buildTree (BOX (llx, lly, rux, ruy), mspnts)
		 val accels =  List.map (fn mspnt => calcAccel (mspnt, tree)) mspnts
	     in
		 List.map applyAccel (ListPair.zip (pts, accels))
	     end
	(* end case *))

  end

structure Main =
  struct

    val dfltN = 10000    (* default number of bodies *)
    val dfltI = 10       (* default number of iterations *)

    structure V = Vector2

    val pi = 3.14159265358979323846

(* random numbers *)
    local
      val seed = ref 0w0
    in
    fun srand s = (seed := Word.fromInt s)
    fun xrand (xl, xh) = let
	  val r = Rand.random (! seed)
	  in
	    seed := r;
	    xl + (((xh - xl) * real (Word.toInt r)) / 2147483647.0)
	  end
    end (* local *)

    val rand = Random.rand (0, 2)

    (* pick a random point on a sphere of specified radius. *)
    fun pickshell rad = 
	let
	    fun pickvec () = 
		let
		    val vec = V.tabulate (fn _ => 1.0 - Random.randReal rand * 2.0)
		    val rsq = V.dotvp(vec, vec)
		in
		    if (rsq > 1.0)
		    then pickvec ()
		    else V.mulvs (vec, rad / Math.sqrt(rsq))
		end
	in
	    pickvec ()
	end

    fun particle (mass, (xp, yp), (xv, yv)) = BarnesHut.PARTICLE(BarnesHut.MP(xp, yp, mass), xv, yv)

    (* generate Plummer model initial conditions for test runs, scaled
     * to units such that M = -4E = G = 1 (Henon, Hegge, etc).
     * See Aarseth, SJ, Henon, M, & Wielen, R (1974) Astr & Ap, 37, 183.
     *)
    fun testdata n =
	let
	    val mfrac = 0.999 (* mass cut off at mfrac of total *)
	    val rn = Real.fromInt n
	    val rsc = (3.0 * pi) / 16.0
	    val vsc = Math.sqrt(1.0 / rsc)
	    fun mkBodies x =
		(case x
		  of (0, cmr, cmv, l) =>
		     let
			 (* offset bodies by normalized cm coordinates.  Also, reverse
			  * the list to get the same order of bodies as in the C version.
			  *)
			 val cmr = V.divvs(cmr, rn)
			 val cmv = V.divvs(cmv, rn)
			 fun norm x =
			     (case x
			       of (nil, l) => l
				| ((mass, pos, vel) :: r, l) => 
				  let
				      val posN = V.subv(pos,cmr)
				      val velN = V.subv(vel,cmv)
				  in
				      norm (r, particle(mass,posN,velN)::l)
				  end)
		     in
			 norm (l, nil)
		     end
		   | (i, cmr, cmv, l) =>
		     let
			 val r = 1.0 / Math.sqrt (Math.pow(xrand(0.0, mfrac), ~2.0/3.0) - 1.0)
			 val pos = pickshell (rsc * r)
			 fun vN () = 
			     let		(* von Neumann technique *)
				 val x = xrand(0.0,1.0)
				 val y = xrand(0.0,0.1)
			     in
				 if (y > x*x * (Math.pow (1.0-x*x, 3.5))) then vN () else x
			     end
			 val v = ((Math.sqrt 2.0) * vN()) / Math.pow(1.0 + r*r, 0.25)
			 val vel = pickshell (vsc * v)
			 val body = (1.0 / rn, pos, vel)
		     in
			 mkBodies (i-1, V.addv(cmr, pos), V.addv(cmv, vel), body :: l)
		     end)
	in
	    mkBodies (n, V.zerov, V.zerov, nil)
	end (* testdata *)
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () = 
		let
		    fun iter (ps, i) =
			if i < dfltI then
			    (* FIXME: read the top-level box from the input *)
			    iter (BarnesHut.oneStep ps, i + 1)
			else
			    ps

		in
		    iter (testdata n, 0)
		end
		
	in
	    RunSeq.run doit
	end

  end

