structure L = List

fun particle2mpnt (PARTICLE (mp, _, _)) = mp

fun plus (x, y) = x+y
fun sum (ls) = L.foldl plus 0.0 ls

fun plusi (x, y) = x+y
fun sumi (ls) = L.foldl plusi 0 ls

fun rmax (x, y) = if x > y then x else y
fun rmin (x, y) = if x < y then x else y

fun maximum (xs) = (case xs
    of nil => (raise Fail "maximum")
     | x :: xs => L.foldl rmax x xs
    (* end case *))

(* Calculate the maximal relative error of the positions of two particle lists
 * (the first argument provides the reference values).
 *
 * PRECONDITION: Both lists are of equal length.
 *)
fun maxErr (rvs, vs) = let    
    fun compare (PARTICLE (MP (_, x1, y1), _, _),
		 PARTICLE (MP (_, x2, y2), _, _)) = let
	val dx = Double.abs(x1-x2)
	val dy = Double.abs(y1-y2)
	val dr = Double.sqrt(dx * dx + dy * dy)
	val r = Double.sqrt(x1 * x1 + y1 * y1)
        in
	    dr / r
        end
    in
       if (L.length(rvs) = L.length(vs))
          then maximum(L.zipWith(compare, rvs, vs))
          else (raise Fail "unequal lengths")
    end

fun moveParticle p = (case p
        of PARTICLE (MP (m, x, y), vx, vy) =>
	   PARTICLE (MP (m, x + vx * dt, y + vy * dt), vx, vy)
        (* end case *))

(*
 * Given a set of particles, move them according to their velocities during a
 *`dt' time interval.
 *)
fun move (ps : particle list) = let 
    fun f (p) = moveParticle p
    in
       L.map f ps
    end

(*
 * Calculate the new velocity of each particle after the time `dt' under the
 * corresponding accelaration in `acs'.
 *)
fun applyAccels (acs, ps) = 
    L.map applyAccel (L.zip(ps, acs))

(* One step of the n-body simulation using the O(n^2) algorithm. 
 *    dt is the time interval.
 *)
fun naiveStep (ps : particle list) = let
    fun allAccels (MP(x, y, m), ps) = let
	fun f (p) = 
	    accel(particle2mpnt(p), x, y, m)
	val (axs, ays) = L.unzip (L.map f ps)
        in
	   (sum(axs), sum(ays))
        end

    fun f (p) = 
	allAccels(particle2mpnt(p), ps)
    val acs =  (L.map f ps)
    val ps' = applyAccels(acs, ps)
    in
       move (ps')
    end

(* utility code for loading the input particles *)
val readreal = PrimIO.readDouble
val readint = PrimIO.readInt
fun readParticles () = let
    val nParticles = readint ()
    fun readVec () = (readreal(), readreal())
    fun readMassPnt () = 
	let
	    val m = readreal()
	    val x = readreal()
	    val y = readreal()
	in 
	    MP (x, y, m)
	end
    fun readParticle () = PARTICLE (readMassPnt(), readreal(), readreal())
    fun doit (i, ps) = if (i>0)
        then doit(i-1, readParticle()::ps)
        else List.rev(ps)
    in
        doit (nParticles, nil)
    end

(*
fun debug () = let
    val nSteps = readint()

    val ps = readParticles()
    val n = L.length(ps)

    fun iter (ps : particle list, i, err) = (raise Fail "todo") (*if (i<nSteps)
        then let
          val bhPs : particle list = Ropes.toSeq(oneStep (llx, lly, rux, ruy, Ropes.fromSeq ps))
	  val naivePs : particle list = naiveStep ps
	  in
             iter(bhPs, i+1, rmax(err, maxErr(naivePs, bhPs)))
          end
        else err*)
*)

fun benchmark () =
    let
	val nSteps = readint()
	fun iter (ps, i) =
	    if i < nSteps then
(* FIXME: read the top-level box from the input *)
		iter (oneStep ps, i + 1)
	    else
		ps
	val particles = fromListP (readParticles ())
	val t0 = Time.now()
	val tree = iter (particles, 0)
	val t = (Time.now() - t0)
    in
	Print.printLn (Time.toString t)
    end

val () = ImplicitThread.runWithGroup(SwpWorkStealing.workGroup(), benchmark)
