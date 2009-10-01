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


structure V = Vector2

val pi = 3.14159265358979323846

(* random numbers *)
local
    val seed = Array64.array (1, 0.0)
in
fun srand s = (Array64.update(seed, 0, Double.fromInt s))
fun xrand (xl, xh) = 
    let
	val r = Rand.randDouble (0.0, Array64.sub(seed, 0))
    in
	Array64.update(seed, 0, r);
	xl + (((xh - xl) * r) / 2147483647.0)
    end
end (* local *)
	 
	 
(* pick a random point on a sphere of specified radius. *)
fun pickshell rad = 
    let
	fun pickvec () = 
	    let
		val vec = V.tabulate (fn _ => 1.0 - Rand.randDouble(0.0, 2.0))
		val rsq = V.dotvp(vec, vec)
	    in
		if (rsq > 1.0)
		then pickvec ()
		else V.mulvs (vec, rad / Double.sqrt(rsq))
	    end
    in
	pickvec ()
    end
    
fun particle (mass, (xp, yp), (xv, yv)) = PARTICLE(MP(xp, yp, mass), xv, yv)
	
(* generate Plummer model initial conditions for test runs, scaled
   * to units such that M = -4E = G = 1 (Henon, Hegge, etc).
   * See Aarseth, SJ, Henon, M, & Wielen, R (1974) Astr & Ap, 37, 183.
   *)
fun testdata n =
    let
	val mfrac = 0.999 (* mass cut off at mfrac of total *)
	val rn = Double.fromInt n
	val rsc = (3.0 * pi) / 16.0
	val vsc = Double.sqrt(1.0 / rsc)
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
		     val r = 1.0 / Double.sqrt (Double.pow(xrand(0.0, mfrac), ~2.0/3.0) - 1.0)
		     val pos = pickshell (rsc * r)
		     fun vN () = 
			 let		(* von Neumann technique *)
			     val x = xrand(0.0,1.0)
			     val y = xrand(0.0,0.1)
			 in
			     if (y > x*x * (Double.pow (1.0-x*x, 3.5))) then vN () else x
			 end
		     val v = ((Double.sqrt 2.0) * vN()) / Double.pow(1.0 + r*r, 0.25)
		     val vel = pickshell (vsc * v)
		     val body = (1.0 / rn, pos, vel)
		 in
		     mkBodies (i-1, V.addv(cmr, pos), V.addv(cmv, vel), body :: l)
		 end)
    in
	mkBodies (n, V.zerov, V.zerov, nil)
    end (* testdata *)

(*
fun debug () = let
    val nSteps = readint()

    val ps = readParticles()
    val n = L.length(ps)

    fun iter (ps : particle list, i, err) = if (i<nSteps)
        then let
          val bhPs : particle list = Ropes.toSeq(oneStep (llx, lly, rux, ruy, Ropes.fromSeq ps))
	  val naivePs : particle list = naiveStep ps
	  in
             iter(bhPs, i+1, rmax(err, maxErr(naivePs, bhPs)))
          end
        else err
    val err = iter(ps, 0, 0.0)
    in
       Print.printLn("Error for BH:"^Double.toString err)
    end
*)

fun benchmark () =
    let
	val nSteps = readint()
	val nBodies = readint()
	val t0 = Time.now()
	val tree = ImplicitThread.runOnWorkGroup(WorkStealing.workGroup(), fn () =>
	    let
		(* use the following code to read the bodies from stdin
		 val particles = fromListP (readParticles ())
		 *)
		val particles = fromListP (testdata nBodies)
		fun iter (ps, i) =
		    if i < nSteps then
			(* FIXME: read the top-level box from the input *)
			iter (oneStep ps, i + 1)
		    else
			ps

	    in
		iter (particles, 0) 
	    end)
	val t = (Time.now() - t0)
    in
	Print.printLn (Time.toString t)
    end

val () = benchmark ()
