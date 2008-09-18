(* barnes-hut.pml
 *
 * Simple Barnes-Hut benchmark borrowed from Chakravarty and Keller.
 *)

(* List size at which parallel map switches to sequential evaluation.
 *)
val seqSz = 1024

structure L = List

fun splitAt (n, ls) = let
    fun split (i, x :: xs, ys) = if (i < n)
        then split(i+1, xs, x :: ys)
        else (L.rev(ys), x :: xs)
    in
        split(0, ls, nil)
    end


(* Map the function f over the list ls in parallel.
 *
 *    PREDCONDITION: n = length(ls)
 *)
fun parMap (f, n, ls) = (case ls
    of nil => nil
     | _ => if (n <= seqSz)
        then L.map(f, ls)
        else let
          val n1 = n div 2
  	  val (xs, ys) = splitAt(n1, ls)
          pval fxs = parMap(f, n1, xs)
	  val fys = parMap(f, n-n1, ys)
          in
	     List.concat(fxs, fys)
          end
   (* end case *))


type real = double
type vector = (real * real)
type point = vector
type veloc = vector
type accel = vector
type area = (point * point)
datatype mass_pnt = MassPnt of (real * point)
datatype particle = Particle of (mass_pnt * veloc)

val sqrt = sqrtd
val abs = absd
val rtos = Double.toString
val readreal = PrimIO.readdouble

fun hd (xs) = (case xs
    of nil => fail "hd"
     | x :: _ => x
    (* end case *))


fun tl (xs) = (case xs
    of nil => fail "tl"
     | _ :: xs => xs
    (* end case *))


fun fst (x, _) = x
fun snd (_, x:bool) = x
fun sndv (_, x:particle) = x

fun compose (f, g) = let
    fun h (x) = f(g(x))
    in
        h
    end


fun rmax (x, y) = if x > y then x else y
fun rmin (x, y) = if x < y then x else y

fun vec2s (x, y) = "("^rtos x^", "^rtos y^") "
fun mp2s (MassPnt (f, pt)) = "MassPnt("^rtos f^", "^vec2s pt^")"
fun part2s (Particle (mp, veloc)) = "Particle("^mp2s mp^", "^vec2s veloc^")"

fun transpose (rows) = (case rows
    of nil => nil
     | _ => let
	fun loop (rows, rows') = (case hd(rows)
 	    of nil => L.rev(L.map(L.rev, rows'))
             |_ => let
              val cols = L.map(hd, rows)
	      val rows'' = L.map(tl, rows)
              in
		  loop(rows'', cols :: rows')
              end
            (* end case *))
         in
	     loop(rows, nil)
         end
    (* end case *))


val gravConst = (*6.670 / 100000000000.0*) 1.0  (* 6.670e-11 *)
val epsilon = 1.0 / 100000000000000000000.0     (* 1.0e-20 *)
(* precision (a cell is *far away* if `l/d < theta') *)
val theta  = 0.8

fun samePt ((x1, y1), (x2, y2)) = 
    abs(x1-x2) < epsilon andalso abs(y1-y2) < epsilon


fun sameMpt (MassPnt(m1, pt1), MassPnt(m2, pt2)) =
    abs(m1-m2) < epsilon andalso samePt(pt1, pt2)


fun particle2mpnt (p) = (case p
    of Particle (mp, _) => mp
    (* end case *))


fun plus (x, y) = x+y
fun sum (ls) = L.foldl plus 0.0 ls

fun plusi (x, y) = x+y
fun sumi (ls) = L.foldl plusi 0 ls

(*
 * Acceleration modulo the gravity constant imposed *by* the first *on* the
 * second.  No acceleration if the position of both points is considered to be
 * equal, i.e., is smaller than `epsilon'.
 *
 * * The second component of the result is `1' if the particles are not to
 *   close for an interaction.
 *)
fun accel (mp1, mp2) = (case (mp1, mp2)
    of (MassPnt (m, (x1, y1)),
	MassPnt (_, (x2, y2))) => let 
	  val dx = x1 - x2
	  val dy = y1 - y2
	  val rsqr = (dx * dx) + (dy * dy)	   
          val r = sqrt(rsqr)
          in
	   if (r < epsilon)
	   then ((0.0, 0.0), 0)
           else let
		   val aabs = m / rsqr
	       in
		   ((aabs * dx / r, aabs * dy / r), 1)
	       end
           end
    (* end case *))


fun applyAccel (dt) = let 
    fun f (p, (ax, ay)) = (case p
        of Particle (mp, (vx, vy)) => 
	   Particle(mp, (vx+ax * dt, vy+ay * dt))
        (* end case *))
    in
      f
    end


(*
 * Calculate the new velocity of each particle after the time `dt' under the
 * corresponding accelaration in `acs'.
 *)
fun applyAccels (dt, acs, ps) = 
    L.map (applyAccel(dt), (L.zip(ps, acs)))


fun moveParticle (dt, p) = (case p
        of Particle (MassPnt (m, (x, y)), (vx, vy)) =>
	   Particle (MassPnt (m, (x + vx * dt, y + vy * dt)), (vx, vy))
        (* end case *))


(*
 * Given a set of particles, move them according to their velocities during a
 *`dt' time interval.
 *)
fun move (dt, ps : particle list) = let 
    fun f (p) = moveParticle(dt, p)
    in
       L.map (f, ps)
    end


(* One step of the n-body simulation using the O(n^2) algorithm. 
 *    dt is the time interval.
 *)
fun naiveStep (dt, ps) = let
    fun allAccels (mp, ps) = let
	fun f (p) = 
	    accel(particle2mpnt(p), mp)
	val (acs, noAcs) = L.unzip (L.map (f, ps))
	val (axs, ays) = L.unzip(acs)
        in
	   ((gravConst * sum(axs), gravConst * sum(ays)),
	    sumi(noAcs))
        end

    fun f (p) = 
	allAccels(particle2mpnt(p), ps)
    val (acs, noAcs) = L.unzip (L.map (f, ps))
    val ps' = applyAccels(dt, acs, ps)
    in
       (move (dt, ps'), sumi(noAcs))
    end


datatype 'a tree = Node of ('a * 'a tree list)

fun cut ((x1, y1), (x2, y2)) = let
    val xm = x1 + (x2 - x1) / 2.0
    val ym = y1 + (y2 - y1) / 2.0
    val a1 = ((x1, y1), (xm, ym))
    val a2 = ((xm, y1), (x2, ym))
    val a3 = ((x1, ym), (xm, y2))
    val a4 = ((xm, ym), (x2, y2))
    in
        (a1, a2, a3, a4)
    end


fun mp (Node (mp, _)) = mp
fun mp1 (MassPnt(mp, _)) = mp
fun particleMP (Particle (mp, _)) = mp

fun centroid (mps : mass_pnt list) = let
    val m = sum(L.map(mp1, mps))
    fun f (MassPnt (m, (x, y))) = (m*x, m*y)
    val (wxs, wys) = L.unzip(L.map(f, mps))
    in
        MassPnt(m, (sum(wxs) / m, sum(wys) / m))
    end


fun lenIsOne (ls) = (case ls
    of nil => fail "lenIsOne"
     | x :: nil => true
     | _ => false
    (* end case *))

fun bhTree (a, ps) =
    if (lenIsOne(ps))
       then Node(hd(ps), nil)
       else let
          val (a1, a2, a3, a4) = cut(a)
	  val (_, (xm, ym)) = a1

	  fun f (MassPnt (_, (x, y))) = (x < xm, y < ym)

	  fun putParticleInQd (p, (ps1, ps2, ps3, ps4)) = let
	      val (fx, fy) = f(p)
	      val ps1 = if (fx andalso fy) then p :: ps1 else ps1
	      val ps2 = if (not(fx) andalso fy) then p :: ps2 else ps2
	      val ps3 = if (fx andalso not(fy)) then p :: ps3 else ps3
	      val ps4 = if (not(fx) andalso not(fy)) then p :: ps4 else ps4
              in
	         (ps1, ps2, ps3, ps4)
              end	      
          val (ps1, ps2, ps3, ps4) = L.foldl putParticleInQd (nil, nil, nil, nil) ps
	  fun f (_, ps) = (case ps
			    of nil => false
			     | x :: xs => true)
	  val children = L.map (bhTree, filter(f, L.zip( a1:: a2:: a3:: a4:: nil,
						   ps1:: ps2:: ps3:: ps4:: nil)))
	  val cd = centroid (L.map(mp, children))

	  in
	      Node (cd, children)
	  end


fun minimum (xs) = (case xs
    of nil => fail "minimum"
     | x :: xs => L.foldl rmin x xs
    (* end case *))


fun maximum (xs) = (case xs
    of nil => fail "maximum"
     | x :: xs => L.foldl rmax x xs
    (* end case *))


(* Calculate the minimal bounding box for the given particle set.
 *
 * PRECONDITION: The particles must contain at least one element.
 *
 * Note: The bounding box is extended by `epsilon' as t represents an open
 *	 interval. 
 *)
fun boundingBox (mps) = let
    fun f (MassPnt (_, xy)) = xy
    val xys = L.map(f, mps)
    val (xs, ys) = L.unzip (xys)
    in
       ((minimum(xs) - epsilon, minimum(ys) - epsilon),
	(maximum(xs) + epsilon, maximum(ys) + epsilon))
    end


(* Returns the maximal side length of an area. *)
fun maxSideLen ((x1,y1), (x2,y2)) = let
    val dx = abs(x1-x2)
    val dy = abs(y1-y2)
    in
        if (dx > dy) then dx else dy
    end


fun isFar (l, MassPnt (_, (x1, y1)), MassPnt (_, (x2, y2))) = let
    val dx = x2-x1
    val dy = y2-y1
    val r = sqrt ((dx * dx) + (dy * dy))
    in
       if (r < epsilon)
          then false
          else l / r < theta
     end


fun superimp (fs) = let
    val (fxs, fys) = L.unzip(fs)
    in
        (sum(fxs), sum(fys))
    end


fun combine arg = (case arg
    of (nil, _, _, zs) => L.rev(zs)
     | (f :: fs, xs, ys, zs) => if f
       then combine(fs, tl(xs), ys, hd(xs)::zs)
       else combine(fs, xs, tl(ys), hd(ys)::zs)
    (* end case *))


fun split (f, xs) = let
    fun loop arg = (case arg
        of (nil, (xs1, xs2)) => (L.rev(xs1), L.rev(xs2))
	 | (x :: xs, (xs1, xs2)) => if (f(x))
	   then loop(xs, (x :: xs1, xs2))
           else loop(xs, (xs1, x :: xs2))
        (* end case *))
    in
       loop(xs, (nil, nil))
    end

(* Calculates the acceleration on a set of particles according to the given
 * Barnes-Hut tree (whose bounding box has a maximum side length as given in
 * the second argument).
 *
 * * In addition to the accelerations, the number of direct and far-field
 *   interactions is computed.
 *)       
fun accels (acs) = (case acs
    of (_, _, nil) => (nil, 0, 0)
     | (Node (crd, nil), len, mps) => let
	   fun f (mp) = accel(crd, mp)
	   val (acs, noAcs) = L.unzip(L.map (f, mps))
           in
	       (acs, sumi(noAcs), 0)
	   end
     | (Node(crd, ts), len, mps) => let
	   fun f (mp) = isFar(len, crd, mp)
	   val direct = L.map(f, mps)
	   val mds = L.zip (mps, direct)
	   val (farMps, closeMps) = split(snd, mds)
	   val (farMps, closeMps) = (L.map(fst, farMps), L.map(fst, closeMps))
	   fun f (mp) = accel(crd, mp)
	   val (farAcs, noFarAcs) = L.unzip (L.map(f, farMps))
	   fun f (t) = accels(t, len / 2.0, closeMps)
	   val (closeAcss, directNos, farNos) = L.unzip3(L.map(f, ts))
	   val closeAcs = L.map(superimp, transpose(closeAcss))
           in
	      (combine(direct, farAcs, closeAcs, nil),
	       sumi(directNos),
	       sumi(farNos) + sumi(noFarAcs))
           end
     (* end case *))


(* Compute the acceleration of a mass particle in the Barnes-Hut tree.
 *)
fun accelOf (ac) = (case ac
    of (Node(crd, nil), len, mp) => fst(accel(crd, mp))
     | (Node(crd, ts), len, mp) => 
       if sameMpt(crd, mp)
          then (0.0, 0.0)
       else if (isFar(len, crd, mp))
          then fst(accel(crd, mp))
       else superimp(L.map(let fun f (t) = accelOf(t, len / 2.0, mp) in f end, ts))
    (* end case *))


(* Execute a single iteration (tree construction, accelaration calculation, and
 * update of the velocities and positions).  The first argument determines the
 * time interval. 
 *
 * PRECONDITION: The particles must contain at least one element.
 *
 * * In addition to the new particles, the number of direct and far-field
 *   interactions is computed.
 *)
fun oneStep' (dt, n, ps) = let
    val mps = L.map(particleMP, ps)
    val box = boundingBox(mps)
    val len = maxSideLen(box)
    val t = bhTree(box, mps)
    val applyAccel = applyAccel(dt)

    fun computeNewParticle (Particle(mp, v)) = let
	val (ax, ay) = accelOf(t, len, mp)
        val acc = (gravConst * ax, gravConst * ay)
        val p = applyAccel(Particle(mp, v), acc)
        in
	  moveParticle(dt, p)
        end
    val ps' = parMap(computeNewParticle, n, ps)
    in
       (ps', 0, 0)
    end


(* Execute a single iteration (tree construction, accelaration calculation, and
 * update of the velocities and positions).  The first argument determines the
 * time interval. 
 *
 * PRECONDITION: The particles must contain at least one element.
 *
 * * In addition to the new particles, the number of direct and far-field
 *   interactions is computed.
 *)
fun oneStep (dt, ps) = let
    val mps = L.map(particleMP, ps)
    val box = boundingBox(mps)
    val len = maxSideLen(box)
    val t = bhTree(box, mps)
    val (preAcs, directNos, farNos) = accels(t, len, mps)
    fun f (ax, ay) = (gravConst * ax, gravConst * ay)
    val acs = L.map(f, preAcs)
    val ps' = applyAccels(dt, acs, ps)
    in
        (move(dt, ps'), directNos, farNos)
    end


(* Calculate the maximal relative error of the positions of two particle lists
 * (the first argument provides the reference values).
 *
 * PRECONDITION: Both lists are of equal length.
 *)
fun maxErr (rvs, vs) = let    
    fun compare (Particle (MassPnt (_, (x1, y1)), _),
		 Particle (MassPnt (_, (x2, y2)), _)) = let
	val dx = abs(x1-x2)
	val dy = abs(y1-y2)
	val dr = sqrt(dx * dx + dy * dy)
	val r = sqrt(x1 * x1 + y1 * y1)
        in
	    dr / r
        end
    in
       if (length(rvs) = length(vs))
          then maximum(L.zipWith(compare, rvs, vs))
          else fail "unequal lengths"
    end


fun readParticles () = let
    val nParticles = readint ()
    fun readVec () = (readreal(), readreal())
    fun readMassPnt () = MassPnt (readreal(), readVec())
    fun readParticle () = Particle (readMassPnt(), readVec())
    fun doit (i, ps) = if (i>0)
        then doit(i-1, readParticle()::ps)
        else L.rev(ps)
    in
        doit (nParticles, nil)
    end


fun timeTest () = let
    val nSteps = readint()
    val particles = readParticles()
    val n = length(particles)
    val dt = 2.0

    fun iter (ps, i) = if (i<nSteps)
        then let
          val (bhPs, dNos, fNos) = oneStep' (dt, n, ps)
          in
             iter(bhPs, i+1)
          end
        else ps


    val _ = Time.timeToEval(let fun f () = iter(particles, 0) in f end)

    in
       ()
    end



fun debug () = let
    val nSteps = readint()
    val dt = 2.0

    val ps = readParticles()
    val n = length(ps)

    fun iter (ps, i, err) = if (i<nSteps)
        then let
          val (bhPs, dNos, fNos) = oneStep' (dt, n, ps)
	  val (naivePs, nNos) = naiveStep(dt, ps)
          val _ = print ("[barnes hut benchmark] number of interactions(naive): "^itos nNos^"\n")
          val _ = print ("[barnes hut benchmark] number of interactions(bh): "^itos dNos^" "^itos fNos^"\n")
	  in
(*
print (concatWith(", ", L.map(part2s, ps))^"\n");
print (concatWith(", ", L.map(part2s, bhPs))^"\n");
print (concatWith(", ", L.map(part2s, naivePs))^"\n");
*)
             iter(bhPs, i+1, rmax(err, maxErr(naivePs,bhPs)))
          end
        else err
    val err = iter(ps, 0, 0.0)
    in
       print("Error for BH:"^rtos err^"\n")
    end


val _ = debug()
