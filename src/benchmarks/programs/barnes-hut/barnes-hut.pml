(* barnes-hut.pml
 * 
 * usage: ./barnes-hut <args>
 *   where <args> can include
 *       -size <int>            number of bodies
 *       -iters <int>           number of iterations
 *) 

structure BarnesHut = struct

structure S = Rope
type 'a seq = 'a S.rope

type scalar = double

(* the particle P (x, y, m, xv, yv) represents a particle in 2d space located *)
(* at point (x, y) with mass m and velocity (xv, yv) *)
datatype particle = P of scalar * scalar * scalar * scalar * scalar

datatype bht =
    BHTLeaf of scalar * scalar * scalar
  | BHTQuad of scalar * scalar * scalar * bht * bht * bht * bht

val map = S.map
val reduce = S.reduce
val filter = S.filter
val size = S.length
val sub = S.sub
val empty = S.empty
fun maxv (x, y) = if x > y then x else y
fun minv (x, y) = if x < y then x else y
fun log4 x = Int.ceilingLg x div 2
fun sq x = x * x
val sqrt = Double.sqrt

val epsilon : scalar = 0.05
val eClose : scalar = 0.01
val dt = 0.025

fun circle_plus ((mx0,my0,m0), (mx1,my1,m1)) = (mx0 + mx1, my0 + my1, m0 + m1)

fun calc_centroid parts = let
  fun f (P (x, y, m, _, _)) = (m * x, m * y, m) 
  val (sum_mx, sum_my, sum_m) = 
    reduce circle_plus (0.0, 0.0, 0.0) (map f parts) 
  in
    (sum_mx / sum_m, sum_my / sum_m, sum_m)
  end

fun in_box (llx, lly, rux, ruy) (P (px, py, _, _, _)) =
  (px > llx) andalso (px <= rux) andalso (py > lly) andalso (py <= ruy)

fun build_bht (box:scalar*scalar*scalar*scalar) (particles:particle seq) = let
  val max_depth = log4 (size particles) - 1 
  fun build (depth, (llx, lly, rux, ruy), particles) =
    if size particles <= 1 orelse depth >= max_depth then
      BHTLeaf (calc_centroid particles)
    else let
      val (midx, midy) = ((llx + rux) / 2.0, (lly + ruy) / 2.0) 
      val b1 = (llx,  lly,  midx,  midy) 
      val b2 = (llx,  midy, midx,  ruy) 
      val b3 = (midx, midy, rux,   ruy)
      val b4 = (midx, lly,  rux,   midy)
      val (pb1, pb2, pb3, pb4) = 
	(| filter (in_box b1) particles,
	   filter (in_box b2) particles,
           filter (in_box b3) particles,
	   filter (in_box b4) particles |)
      val depth' = depth + 1
      val (q1, q2, q3, q4) =
	 (| build (depth', b1, pb1),
	    build (depth', b2, pb2),
	    build (depth', b3, pb3),
	    build (depth', b4, pb4) |)
      val (cx, cy, cm) = calc_centroid particles
      in
        BHTQuad (cx, cy, cm, q1, q2, q3, q4)
      end
  in
    build (0, box, particles)
  end

fun accel (x1, y1, _) (x2, y2, m) = let
  val dx   = x1 - x2
  val dy   = y1 - y2
  val rsqr = sq dx + sq dy
  val r    = sqrt rsqr 
  in
    if r < epsilon then
      (0.0, 0.0)
    else let
      val aabs = m / rsqr 
      in (aabs * dx / r , aabs * dy / r) end
  end

fun is_close (x1, y1, m) (x2, y2) =  
  sq (x1 - x2) + sq (y1 - y2) < eClose

fun calc_accel (P (x, y, m, _, _)) bht = let
  val mpt = (x, y, m)
  fun aux bht =
   (case bht of
     BHTLeaf (x, y, m) =>
       accel mpt (x, y, m)
   | BHTQuad (x, y, m, q1, q2, q3, q4) =>
       if is_close mpt (x, y) then let
	 val ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) =
	   (aux q1, aux q2, aux q3, aux q4)
	 in
	   (x1 + x2 + x3 + x4, y1 + y2 + y3 + y4)
	 end
       else
	 accel mpt (x, y, m))
   in aux bht end

fun move_particle (P (x, y, m, vx, vy)) (ax, ay) =
  P (x + vx, y + vy, m, vx + ax, vy + ay)

fun get_x (P (x, _, _, _, _)) = x
fun get_y (P (_, y, _, _, _)) = y

fun one_step parts =
  if size parts = 0 then
    empty ()
  else let
    val P (x0, y0, _, _, _) = sub (parts, 0)
    val box0 = (reduce minv x0 (map get_x parts) - epsilon,
		reduce minv y0 (map get_y parts) - epsilon,
		reduce maxv x0 (map get_x parts) + epsilon,
		reduce maxv y0 (map get_y parts) + epsilon)
    val tree = build_bht box0 parts
    fun f part = move_particle part (calc_accel part tree)
    in
      map f parts
    end

end

structure Main =
  struct

    val dfltN = 200000   (* default number of bodies *)
    val dfltI = 20       (* default number of iterations *)

    fun getSizeArg args =
	(case args
	  of arg1 :: arg2 :: args =>
	     if String.same (arg1, "-size") then Int.fromString arg2
	     else getSizeArg (arg2 :: args)
	   | _ => NONE
	(* end case *))

    fun getNumItersArg args =
	(case args
	  of arg1 :: arg2 :: args =>
	     if String.same (arg1, "-iters") then Int.fromString arg2
	     else getNumItersArg (arg2 :: args)
	   | _ => NONE
	(* end case *))

    structure V = Vector2
    structure BH = BarnesHut

    fun particle (mass, (xp, yp), (xv, yv)) = BarnesHut.P (xp, yp, mass, xv, yv)
    fun genBodies n = List.map particle (GenBodies.testdata n)

    val epsilon = 0.0000000001
    fun bumpParticle (BarnesHut.P (xp, yp, mass, xv, yv)) =
	BarnesHut.P (xp+epsilon, yp+epsilon, mass+epsilon, xv+epsilon, yv+epsilon)

    fun readFromFile () =
	let
	    val f = TextIO.openIn "../../input-data/bodies.txt"
	    val SOME nParticles = Int.fromString (Option.valOf (TextIO.inputLine f))
	    fun rd d = Option.valOf (Double.fromString d)
	    fun lp acc =
		(case TextIO.inputLine f
		  of NONE => List.rev acc
		   | SOME line => 
		     let
			 val xp::yp::mass::xv::yv::nil = List.map rd (String.tokenize " " line)
		     in
			 lp(particle(mass, (xp, yp), (xv, yv)) :: acc)
		     end)
	in
	    lp nil
	end
	
    fun main (_, args) =
	let
	    val bodiesList = (case getSizeArg args of NONE => readFromFile() | SOME n => genBodies n)
	    val nIters = (case getNumItersArg args of NONE => dfltI | SOME i => i)
	    (* the map below has the effect of distributing the parallel array across per-vproc nurseries, thereby
	     * distributing subsequent GC load
	     *)
	    val bodiesArray = RunPar.runSilent (fn _ => Rope.map bumpParticle (Rope.fromList bodiesList))
	    fun doit () = 
		let
		    fun iter (ps, i) =
			if i < nIters then
			    iter (BarnesHut.one_step ps, i + 1)
			else
			    ps

		in
		    iter (bodiesArray, 0)
		end
		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
