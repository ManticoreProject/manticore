(* barnes-hut.pml
 * 
 * Barnes Hut computation. This code translated almost exactly from the Data-parallel
 * Haskell benchmark.
 * http://darcs.haskell.org/packages/ndp/examples/barnesHut/
 *) 

datatype bounding_box = BOX of (double * double * double * double)
datatype mass_point = MP of (double * double * double) (* xpos ypos mass *)
datatype particle = PARTICLE of (mass_point * double * double) (* mass point and velocity *)
datatype bh_tree 
  = BHT_QUAD of (double * double * double *     (* root mass point *)
		 bh_tree * bh_tree * bh_tree * bh_tree)
                                                (* subtrees *)
  | BHT_SINGLETON of (double * double * double) (* root mass point *)
  | BHT_EMPTY

val epsilon : double = 0.05
val eClose : double = 0.01
val dt = 2.0

fun applyAccel (PARTICLE (mp, vx, vy), (ax, ay)) = PARTICLE (mp, vx+ax * dt, vy+ay * dt)

fun isClose (MP (x1, y1, m), x2, y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) < eClose

fun accel (MP (x1,  y1, _), x2, y2, m)  =
    let
	val dx   = x1 - x2 
	val dy   = y1 - y2 
	val rsqr = (dx * dx) + (dy * dy) 
	val r    = Double.sqrt rsqr 
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
      of BHT_EMPTY => 
	 (0.0, 0.0)
       | BHT_SINGLETON (x, y, m) =>
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

fun calcCentroid (mpts : mass_point list) (*: mass_point*) = 
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
fun buildTree (BOX (llx, lly, rux, ruy), nParticles, particles : mass_point list) (*: bh_tree*) =
    if nParticles = 0 then
	BHT_EMPTY
    else if nParticles = 1 then
	let
	    val MP (x, y, m) = calcCentroid particles
	in
	    BHT_SINGLETON (x, y, m)
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
	    val (q1, q2, q3, q4) =
		( buildTree (b1, n1, p1),
		  buildTree (b2, n2, p2),
		  buildTree (b3, n3, p3),
		  buildTree (b4, n4, p4) )
	in
	    BHT_QUAD (x, y, m, q1, q2, q3, q4)
	end

fun oneStep (llx, lly, rux, ruy, pts : particle list) (*: particle list *) =
    let
	val mspnts = List.map (fn (PARTICLE (mpnt, _, _)) => mpnt) pts
	val tree = buildTree (BOX (llx, lly, rux, ruy), List.length pts, mspnts)
	val accels =  List.map (fn mspnt => calcAccel (mspnt, tree)) mspnts
    in
	List.map applyAccel (List.zip (pts, accels))
    end
