(* barnes-hut.pml
 *
 * Simple Barnes-Hut benchmark borrowed from Chakravarty and Keller.
 *)

type vector = (float * float);
type point = vector;
type veloc = vector;
type accel = vector;
type area = (point * point);
datatype mass_pnt = MassPnt of (float * point);
datatype particle = Particle of (mass_pnt * veloc);

fun hd (xs) = (case xs
    of nil => fail "hd"
     | x :: _ => x
    (* end case *))
;

fun tl (xs) = (case xs
    of nil => fail "tl"
     | _ :: xs => xs
    (* end case *))
;

fun fst (x, _) = x;
fun snd (_, x:bool) = x;
fun sndv (_, x:particle) = x;

fun compose (f, g) = let
    fun h (x) = f(g(x))
    in
        h
    end
;

fun fmax (x, y) = if x > y then x else y;
fun fmin (x, y) = if x < y then x else y;

fun vec2s (x, y) = "("^ftos x^", "^ftos y^") ";
fun mp2s (MassPnt (f, pt)) = "MassPnt("^ftos f^", "^vec2s pt^")";
fun part2s (Particle (mp, veloc)) = "Particle("^mp2s mp^", "^vec2s veloc^")";

fun zip (xs, ys) = let
    fun loop (xs, ys, zs) = (case (xs, ys)
        of (nil, _) => rev(zs)
	 | (_, nil) => rev(zs)
	 | (x :: xs, y :: ys) => loop(xs, ys, (x, y) :: zs)
        (* end case *))
     in
        loop(xs, ys, nil)
     end
;

fun unzip (xs) = let
    fun loop (xs, (zs1, zs2)) = (case xs
        of nil => (rev(zs1), rev(zs2))
	 | (x1, x2) :: xs => loop(xs, (x1 :: zs1, x2 :: zs2))
        (* end case *))
     in
        loop(xs, (nil, nil))
     end
;

fun unzip3 (xs) = let
    fun loop (xs, (zs1, zs2, zs3)) = (case xs
        of nil => (rev(zs1), rev(zs2), rev(zs3))
	 | (x1, x2, x3) :: xs => loop(xs, (x1 :: zs1, x2 :: zs2, x3 :: zs3))
        (* end case *))
     in
        loop(xs, (nil, nil, nil))
     end
;

fun filter (f, ls) = let
    fun loop arg = (case arg
        of (nil, res) => rev(res)
	 | (x :: xs, res) => loop(xs, if f(x) then x :: res else res)
        (* end case *))
    in
       loop(ls, nil)
    end
;

fun transpose (rows) = (case rows
    of nil => nil
     | _ => let
	fun loop (rows, rows') = (case hd(rows)
 	    of nil => rev(map(rev, rows'))
             |_ => let
              val cols = map(hd, rows)
	      val rows'' = map(tl, rows)
              in
		  loop(rows'', cols :: rows')
              end
            (* end case *))
         in
	     loop(rows, nil)
         end
    (* end case *))
;

val gravConst : float = 1.0 (*6.670 / 100000000000.0*);  (* 6.670e-11 *)
val epsilon = 1.0 / 100000000000000000000.0;     (* 1.0e-20 *)
(* precision (a cell is *far away* if `l/d < theta') *)
val theta  = 0.8;

fun samePt ((x1, y1), (x2, y2)) = 
    absf(x1-x2) < epsilon andalso absf(y1-y2) < epsilon
;

fun sameMpt (MassPnt(m1, pt1), MassPnt(m2, pt2)) =
    absf(m1-m2) < epsilon andalso samePt(pt1, pt2)
;

fun particle2mpnt (p) = (case p
    of Particle (mp, _) => mp
    (* end case *))
;

fun plus (x, y) = x+y;
fun sum (ls) = foldl (plus, 0.0, ls);

fun plusi (x, y) = x+y;
fun sumi (ls) = foldl (plusi, 0, ls);

fun zipWith (oper, xs, ys) = map(oper, zip(xs, ys));

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
          val r = sqrtf(rsqr)
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
;

fun applyAccel (dt) = let 
    fun f (p, (ax, ay)) = (case p
        of Particle (mp, (vx, vy)) => 
	   Particle(mp, (vx+ax * dt, vy+ay * dt))
        (* end case *))
    in
      f
    end
;

(*
 * Calculate the new velocity of each particle after the time `dt' under the
 * corresponding accelaration in `acs'.
 *)
fun applyAccels (dt, acs, ps) = 
    map (applyAccel(dt), (zip(ps, acs)))
;

fun moveParticle (dt, p) = (case p
        of Particle (MassPnt (m, (x, y)), (vx, vy)) =>
	   Particle (MassPnt (m, (x + vx * dt, y + vy * dt)), (vx, vy))
        (* end case *))
;

(*
 * Given a set of particles, move them according to their velocities during a
 *`dt' time interval.
 *)
fun move (dt, ps : particle list) = let 
    fun f (p) = moveParticle(dt, p)
    in
       map (f, ps)
    end
;

(* One step of the n-body simulation using the O(n^2) algorithm. 
 *    dt is the time interval.
 *)
fun naiveStep (dt, ps) = let
    fun allAccels (mp, ps) = let
	fun f (p) = 
	    accel(particle2mpnt(p), mp)
	val (acs, noAcs) = unzip (map (f, ps))
	val (axs, ays) = unzip(acs)
        in
	   ((gravConst * sum(axs), gravConst * sum(ays)),
	    sumi(noAcs))
        end

    fun f (p) = 
	allAccels(particle2mpnt(p), ps)
    val (acs, noAcs) = unzip (map (f, ps))
    val ps' = applyAccels(dt, acs, ps)
    in
       (move (dt, ps'), sumi(noAcs))
    end
;

datatype 'a tree = Node of ('a * 'a tree list);

fun cut ((x1:float, y1:float), (x2:float, y2:float)) = let
    val xm = x1 + (x2 - x1) / 2.0
    val ym = y1 + (y2 - y1) / 2.0
    val a1 = ((x1, y1), (xm, ym))
    val a2 = ((xm, y1), (x2, ym))
    val a3 = ((x1, ym), (xm, y2))
    val a4 = ((xm, ym), (x2, y2))
    in
        (a1, a2, a3, a4)
    end
;

fun mp (Node (mp, _)) = mp;
fun mp1 (MassPnt(mp, _)) = mp;
fun particleMP (Particle (mp, _)) = mp;

fun centroid (mps : mass_pnt list) = let
    val m = sum(map(mp1, mps))
    fun f (MassPnt (m, (x, y))) = (m*x, m*y)
    val (wxs, wys) = unzip(map(f, mps))
    in
        MassPnt(m, (sum(wxs) / m, sum(wys) / m))
    end
;

fun bhTree (a, ps) =
    if (length(ps) = 1)
       then Node(hd(ps), nil)
       else let
          val (a1, a2, a3, a4) = cut(a)
	  val (_, (xm, ym)) = a1

	  fun f (MassPnt (_, (x, y))) = (x < xm, y < ym)
	  val flags = map (f, ps)
	  val pzs = zip(ps, flags)
		    
	  fun f1 (_, (fx, fy)) = fx andalso fy
	  val ps1 = map(fst,  filter(f1, pzs))
	  fun f2 (_, (fx, fy)) = not (fx) andalso fy
	  val ps2 = map(fst, filter(f2, pzs))
	  fun f3 (_, (fx, fy)) = fx andalso not(fy)
	  val ps3 = map(fst, filter(f3, pzs))
	  fun f4 (_, (fx, fy)) = not(fx) andalso not(fy)
	  val ps4 = map(fst, filter(f4, pzs))
		    
	  fun f (_, ps) = (case ps
			    of nil => false
			     | x :: xs => true)
	  val childs = map (bhTree, filter(f, zip( a1:: a2:: a3:: a4:: nil,
						   ps1:: ps2:: ps3:: ps4:: nil)))
	  val cd = centroid (map(mp, childs))

	  in
	      Node (cd, childs)
	  end
;

fun minimum (xs) = (case xs
    of nil => fail "minimum"
     | x :: xs => foldl(fmin, x, xs)
    (* end case *))
;

fun maximum (xs) = (case xs
    of nil => fail "maximum"
     | x :: xs => foldl(fmax, x, xs)
    (* end case *))
;

(* Calculate the minimal bounding box for the given particle set.
 *
 * PRECONDITION: The particles must contain at least one element.
 *
 * Note: The bounding box is extended by `epsilon' as t represents an open
 *	 interval. 
 *)
fun boundingBox (mps) = let
    fun f (MassPnt (_, xy)) = xy
    val xys = map(f, mps)
    val (xs, ys) = unzip (xys)
    in
       ((minimum(xs) - epsilon, minimum(ys) - epsilon),
	(maximum(xs) + epsilon, maximum(ys) + epsilon))
    end
;

(* Returns the maximal side length of an area. *)
fun maxSideLen ((x1,y1), (x2,y2)) = let
    val dx = absf(x1-x2)
    val dy = absf(y1-y2)
    in
        if (dx > dy) then dx else dy
    end
;

fun isFar (l, MassPnt (_, (x1, y1)), MassPnt (_, (x2, y2))) = let
    val dx = x2-x1
    val dy = y2-y1
    val r = sqrtf ((dx * dx) + (dy * dy))
    in
       if (r < epsilon)
          then false
          else l / r < theta
     end
;

fun superimp (fs) = let
    val (fxs, fys) = unzip(fs)
    in
        (sum(fxs), sum(fys))
    end
;

fun combine arg = (case arg
    of (nil, _, _, zs) => rev(zs)
     | (f :: fs, xs, ys, zs) => if f
       then combine(fs, tl(xs), ys, hd(xs)::zs)
       else combine(fs, xs, tl(ys), hd(ys)::zs)
    (* end case *))
;

fun split (f, xs) = let
    fun loop arg = (case arg
        of (nil, (xs1, xs2)) => (rev(xs1), rev(xs2))
	 | (x :: xs, (xs1, xs2)) => if (f(x))
	   then loop(xs, (x :: xs1, xs2))
           else loop(xs, (xs1, x :: xs2))
        (* end case *))
    in
       loop(xs, (nil, nil))
    end
;

(* Calculates the acceleration on a set of particles according to the given
 * Barnes-Hut tree (whose bounding box has a maximum side length as given in
 * the second argument).
 *
 * * In addition to the accelerations, the number of direct and far-field
 *   interactions is computed.
 *)       
fun accels (acs: (mass_pnt tree * float * mass_pnt list)) = (case acs
    of (_, _, nil) => (nil, 0, 0)
     | (Node (crd, nil), len, mps) => let
	   fun f (mp) = accel(crd, mp)
	   val (acs, noAcs) = unzip(map (f, mps))
           in
	       (acs, sumi(noAcs), 0)
	   end
     | (Node(crd, ts), len, mps) => let
	   fun f (mp) = isFar(len, crd, mp)
	   val direct = map(f, mps)
	   val mds = zip (mps, direct)
	   val (farMps, closeMps) = split(snd, mds)
	   val (farMps, closeMps) = (map(fst, farMps), map(fst, closeMps))
	   fun f (mp) = accel(crd, mp)
	   val (farAcs, noFarAcs) = unzip (map(f, farMps))
	   fun f (t) = accels(t, len / 2.0, closeMps)
	   val (closeAcss, directNos, farNos) = unzip3(map(f, ts))
	   val closeAcs = map(superimp, transpose(closeAcss))
           in
	      (combine(direct, farAcs, closeAcs, nil),
	       sumi(directNos),
	       sumi(farNos) + sumi(noFarAcs))
           end
     (* end case *))
;

(* Compute the acceleration of a mass particle in the Barnes-Hut tree.
 *)
fun accelOf (ac : (mass_pnt tree * float * mass_pnt)) = (case ac
    of (Node(crd, nil), len, mp) => fst(accel(crd, mp))
     | (Node(crd, ts), len, mp) => 
       if sameMpt(crd, mp)
          then (0.0, 0.0)
       else if (isFar(len, crd, mp))
          then fst(accel(crd, mp))
       else superimp(map(let fun f (t) = accelOf(t, len / 2.0, mp) in f end, ts))
    (* end case *))
;

fun splitAt (n, ls) = let
    fun split (i, x :: xs, ys) = if (i < n)
        then split(i+1, xs, x :: ys)
        else (rev(ys), x :: xs)
    in
        split(0, ls, nil)
    end
;

(* List size at which parallel map switches to sequential evaluation.
 *)
val seqSz = 1;

(* Map the function f over the list ls in parallel.
 *
 *    PREDCONDITION: n = length(ls)
 *)
fun parMap (f, n, ls) = (case ls
    of nil => nil
     | _ => if (n <= seqSz)
        then map(f, ls)
        else let
          val n1 = n div 2
  	  val (xs, ys) = splitAt(n1, ls)
          dval fxs = parMap(f, n1, xs)
	  val fys = parMap(f, n-n1, ys)
          in
	     fxs @ fys
          end
   (* end case *))
;

(* Execute a single iteration (tree construction, accelaration calculation, and
 * update of the velocities and positions).  The first argument determines the
 * time interval. 
 *
 * PRECONDITION: The particles must contain at least one element.
 *
 * * In addition to the new particles, the number of direct and far-field
 *   interactions is computed.
 *)
fun oneStep' (dt, ps) = let
    val mps = map(particleMP, ps)
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
    in
       (parMap(computeNewParticle, length(ps), ps), 0, 0)
    end
(*
    val preAcs = map(let fun f (mp) = accelOf(t, len, mp) in f end, mps)
    fun f (ax, ay) = (gravConst * ax, gravConst * ay)
    val acs = map(f, preAcs)
    val ps' = applyAccels(dt, acs, ps)
    in
        (move(dt, ps'), 0, 0)
    end     
*)
;

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
    val mps = map(particleMP, ps)
    val box = boundingBox(mps)
    val len = maxSideLen(box)
    val t = bhTree(box, mps)
    val (preAcs, directNos, farNos) = accels(t, len, mps)
    fun f (ax, ay) = (gravConst * ax, gravConst * ay)
    val acs = map(f, preAcs)
    val ps' = applyAccels(dt, acs, ps)
    in
        (move(dt, ps'), directNos, farNos)
    end
;

(* Calculate the maximal relative error of the positions of two particle lists
 * (the first argument provides the reference values).
 *
 * PRECONDITION: Both lists are of equal length.
 *)
fun maxErr (rvs, vs) = let    
    fun compare (Particle (MassPnt (_, (x1, y1)), _),
		 Particle (MassPnt (_, (x2, y2)), _)) = let
	val dx = absf(x1-x2)
	val dy = absf(y1-y2)
	val dr = sqrtf(dx * dx + dy * dy)
	val r = sqrtf(x1 * x1 + y1 * y1)
        in
	    dr / r
        end
    in
       if (length(rvs) = length(vs))
          then maximum(zipWith(compare, rvs, vs))
          else fail "unequal lengths"
    end
;

fun readParticles () = let
    val nParticles = readint ()
    fun readVec () = (readfloat(), readfloat())
    fun readMassPnt () = MassPnt (readfloat(), readVec())
    fun readParticle () = Particle (readMassPnt(), readVec())
    fun doit (i, ps) = if (i>0)
        then doit(i-1, readParticle()::ps)
        else rev(ps)
    in
        doit (nParticles, nil)
    end
;

fun debug () = let
    val nSteps = 1
    val dt = 2.0

    val ps = readParticles()

    fun iter (ps, i, err) = if (i<nSteps)
        then let
          val (bhPs, dNos, fNos) = oneStep' (dt, ps)
	  val (naivePs, nNos) = naiveStep(dt, ps)
          val _ = print ("[barnes hut benchmark] number of interactions(naive): "^itos nNos^"\n");
          val _ = print ("[barnes hut benchmark] number of interactions(bh): "^itos dNos^" "^itos fNos^"\n");
	  in
(*
print (concatWith(", ", map(part2s, ps))^"\n");
print (concatWith(", ", map(part2s, bhPs))^"\n");
print (concatWith(", ", map(part2s, naivePs))^"\n");
*)
             iter(bhPs, i+1, fmax(err, maxErr(naivePs,bhPs)))
          end
        else err
    val err = iter(ps, 0, 0.0)
    in
       print("Error for BH:"^ftos err^"\n")
    end
;

val ex1 = 
    Particle (MassPnt(1.0, (2.0, 3.0)), (~1.0, 2.0)) ::
    Particle (MassPnt(2.0, (2.0, 1.0)), (1.0, 0.0)) ::
    Particle (MassPnt(2.0, (12.0, 1.0)), (1.0, 30000.0)) ::
    nil
;

debug()
