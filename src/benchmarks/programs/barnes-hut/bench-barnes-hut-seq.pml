structure L = List

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

fun maximum (xs) = (case xs
    of nil => (raise Fail "maximum")
     | x :: xs => L.foldl Double.max x xs
    (* end case *))

(* Calculate the maximal relative error of the positions of two particle lists
 * (the first argument provides the reference values).
 *
 * PRECONDITION: Both lists are of equal length.
 *)
fun maxErr (rvs, vs) = let    
    fun compare (PARTICLE (MP (x1, y1, _), _, _),
		 PARTICLE (MP (x2, y2, _), _, _)) = let
	val dx = Double.abs(x1-x2)
	val dy = Double.abs(y1-y2)
	val dr = Double.sqrt(dx * dx + dy * dy)
	val r = Double.sqrt(x1 * x1 + y1 * y1)
        in
	    dr / r
        end
    in
       if (List.length(rvs) = List.length(vs))
          then maximum(List.zipWith(compare, rvs, vs))
          else (raise Fail "unequal lengths")
    end

fun debug () = let
    val nSteps = readint()

    val ps = readParticles()
    val n = L.length(ps)

    fun iter (ps : particle list, i, err) = if (i<nSteps)
        then let
          val bhPs : particle list = oneStep ps
	  val naivePs : particle list = naiveStep (dt, ps)
	  in
             iter(bhPs, i+1, Double.max(err, maxErr(naivePs, bhPs)))
          end
        else err
    val err = iter(ps, 0, 0.0)
    in
       Print.printLn("Error for BH:"^Double.toString err)
    end

fun benchmark () =
    let
	val nSteps = readint()
	fun iter (ps, i) =
	    if i < nSteps then
(* FIXME: read the top-level box from the input *)
		iter (oneStep ps, i + 1)
	    else
		ps
	val particles = readParticles ()
	val t0 = Time.now()
	val tree = iter (particles, 0)
	val t = (Time.now() - t0)
    in
	Print.printLn (Time.toString t)
    end

val () = benchmark ()
