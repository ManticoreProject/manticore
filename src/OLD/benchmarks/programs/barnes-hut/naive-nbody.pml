

  val gravConst = (*6.670 / 100000000000.0*) 1.0  (* 6.670e-11 *)

  (*
   * Acceleration modulo the gravity constant imposed *by* the first *on* the
   * second.  No acceleration if the position of both points is considered to be
   * equal, i.e., is smaller than `epsilon'.
   *
   * * The second component of the result is `1' if the particles are not to
   *   close for an interaction.
   *)
  fun accel (mp1, mp2) = (case (mp1, mp2)
      of (MP (x1, y1, m),
	  MP (x2, y2, _)) => let 
	    val dx = x1 - x2
	    val dy = y1 - y2
	    val rsqr = (dx * dx) + (dy * dy)	   
	    val r = Double.sqrt(rsqr)
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


  fun particle2mpnt (p) = (case p
      of PARTICLE (mp, _, _) => mp
      (* end case *))

  fun plus (x, y) = x+y
  fun sum (ls) = List.foldl plus 0.0 ls

  fun plusi (x, y) = x+y
  fun sumi (ls) = List.foldl plusi 0 ls

  fun moveParticle (dt, p) = (case p
	  of PARTICLE (MP (x, y, m), vx, vy) =>
	     PARTICLE (MP (x + vx * dt, y + vy * dt, m), vx, vy)
	  (* end case *))

  (*
   * Given a set of particles, move them according to their velocities during a
   *`dt' time interval.
   *)
  fun move (dt, ps : particle list) = let 
      fun f (p) = moveParticle(dt, p)
      in
	 List.map f ps
      end

  fun applyAccel (dt) = let 
    fun f (p, (ax, ay)) = (case p
        of PARTICLE (mp, vx, vy) => 
	   PARTICLE(mp, vx+ax * dt, vy+ay * dt)
        (* end case *))
    in
      f
    end

  (*
   * Calculate the new velocity of each particle after the time `dt' under the
   * corresponding accelaration in `acs'.
   *)
  fun applyAccels (dt, acs, ps) = 
      List.map (applyAccel(dt)) (List.zip(ps, acs))


  (* One step of the n-body simulation using the O(n^2) algorithm. 
   *    dt is the time interval.
   *)
  fun naiveStep (dt, ps) = let
      fun allAccels (mp, ps) = let
	  fun f (p) = 
	      accel(particle2mpnt(p), mp)
	  val (acs, noAcs) = ListPair.unzip (List.map f ps)
	  val (axs, ays) = ListPair.unzip(acs)
	  in
	     ((gravConst * sum(axs), gravConst * sum(ays)),
	      sumi(noAcs))
	  end

      fun f (p) = 
	  allAccels(particle2mpnt(p), ps)
      val (acs, noAcs) = ListPair.unzip (List.map f ps)
      val ps' = applyAccels(dt, acs, ps)
      in
	 move (dt, ps')
      end


