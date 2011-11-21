structure GenBodies =
  struct

    structure V = Vector2

    val pi = 3.14159265358979323846

    (* random numbers *)
    fun xrand (xl, xh) = 
	let
	    val r = Rand.randDouble (0.0, 1000000000.0)
	in
	    xl + (((xh - xl) * r) / 2147483647.0)
	end

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
				      norm (r, (mass,posN,velN)::l)
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

  end
