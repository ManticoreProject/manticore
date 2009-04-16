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

val reduceP = Ropes.reduceP
val filterP = Ropes.filterP
val fromListP = Ropes.fromList
val lengthP = Ropes.length
val mapP = Ropes.mapP
type 'a parray = 'a Ropes.rope

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
		     (| calcAccel (mpt, st1), 
		        calcAccel (mpt, st2), 
		        calcAccel (mpt, st3), 
		        calcAccel (mpt, st4) |)
	     in
		 (x1 + x2 + x3 + x4, y1 + y2 + y3 + y4)
	     end
	 else
	     accel (mpt, x, y, m)
    (* end case *))

fun calcCentroid (mpts : mass_point parray) (*: mass_point*) = 
    let
        fun circlePlus ((mx0,my0,m0), (mx1,my1,m1)) = (mx0+mx1, my0+my1, m0+m1)
	val (sum_mx, sum_my, sum_m) = 
	    reduceP (circlePlus, (0.0, 0.0, 0.0), [| (m*x, m*y, m) | MP (x, y, m) in mpts |])
    in
	MP (sum_mx / sum_m, sum_my / sum_m, sum_m)
    end

fun inBox (BOX (llx, lly, rux, ruy)) (MP (px, py, _)) =
    (px > llx) andalso (px <= rux) andalso (py > lly) andalso (py <= ruy)

(* split mass points according to their locations in the quadrants *)
fun buildTree (BOX (llx, lly, rux, ruy), particles : mass_point parray) (*: bh_tree*) =
    if lengthP particles = 0 then
	BHT_EMPTY
    else if lengthP particles = 1 then
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
	    val (q1, q2, q3, q4) =
		(| buildTree (b1, filterP (inBox b1, particles)),
		   buildTree (b2, filterP (inBox b2, particles)),
		   buildTree (b3, filterP (inBox b3, particles)),
		   buildTree (b4, filterP (inBox b4, particles)) |)
	in
	    BHT_QUAD (x, y, m, q1, q2, q3, q4)
	end

fun oneStep (llx, lly, rux, ruy, pts : particle parray) (*: particle parray *) =
    let
	val mspnts = [| mpnt | PARTICLE (mpnt, _, _) in pts |]
	val tree = buildTree (BOX (llx, lly, rux, ruy), mspnts)
	val accels =  [| calcAccel (mspnt, tree) | mspnt in mspnts |]
    in
	[| applyAccel (pt, acc) | pt in pts, acc in accels |]
    end
