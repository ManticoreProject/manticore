(* mandelbrot.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Sequential version of mandelbrot-set computation. The output goes to out.ppm.
 *)

structure Mandelbrot =
  struct

    val xBase = ~2.0
    val yBase = 1.25
    val side = 2.5

    val maxCount = 1000

    fun pix2rgb cnt = 
	if cnt >= maxCount then
	    (0.0, 0.0, 0.0)
	else let
		val w = Real.fromInt cnt / (Real.fromInt (maxCount-1))
	    in
		(w, w, 0.25 + w*0.75)
	    end

    fun mandelbrot N = let
	  fun elt (i, j) = let
	        val delta = side / (Real.fromInt (N-1))
		val c_re = xBase + (delta * Real.fromInt j)
		val c_im = yBase - (delta * Real.fromInt i)
		fun loop (cnt, z_re, z_im) = 
		    if (cnt < maxCount)
		      then let
			val z_re_sq = z_re * z_re
			val z_im_sq = z_im * z_im
			in
			  if ((z_re_sq + z_im_sq) >= 4.0)
			    then cnt
			    else let
			      val z_re_im = z_re * z_im
			      in
				loop(cnt+1, (z_re_sq - z_im_sq) + c_re, z_re_im + z_re_im + c_im)
			      end
			end
		      else cnt
		in
		  loop (0, c_re, c_im)
		end
	  val tab = Vector.tabulate
          val counts = tab (N, fn i => tab (N, fn j => elt (i, j)))
(*
	  val image = Array.array (N*N, (0.0,0.0,0.0))
	  fun output (i, j, (r, g, b)) = Array.update (image, i*N + j, (r, g, b));
	  fun outputImg i = if i < N
		  then let
		    fun loop j = if j < N
			then (
			     output (i, j, pix2rgb (elt (i, j)));
			     loop (j+1)
			  )
			else outputImg (i+1)
		    in
		       loop 0
		    end
		  else ()
*)
	  in
	    counts
	  end

  end

structure Main = struct

  structure V = Vector

  val dfltN = 256

  fun tellMeAbout nss = let
    fun row ns = let
      val len = V.length ns
      fun sub i = V.sub (ns, i)
      fun lp (i, acc) = 
        if (i >= len) then
          String.concatWith "," (List.rev acc)
        else let
          val s = Int.toString (sub i)
          in
            lp (i+1, s::acc)
          end
      in
        lp (0, [])
      end
    val len = V.length nss
    fun sub i = V.sub (nss, i)
    fun lp (i, acc) = 
      if (i >= len) then 
        String.concatWith "\n" (List.rev acc)
      else let
        val s = row (sub i)
        in
          lp (i+1, s::acc)
        end      
    fun println s = (TextIO.print s; TextIO.print "\n")
    in
      println (lp (0, []))
    end

  fun getArgs args = let
    fun lp (args, chatty, size) = (case args
      of s::ss =>
           if (s = "-v") then
             lp (ss, true, size)
           else if (s = "-size") then (case ss
             of s'::ss' => lp (ss', chatty, Int.fromString s')
              | nil => lp ([], chatty, SOME dfltN)
             (* end case *))
           else (* breeze past other options; could be used elsewhere *)
             lp (ss, chatty, size)
       | nil => (case size
           of NONE => (chatty, dfltN)
            | SOME sz => (chatty, sz)
           (* end case *))
      (* end case *))
    in
      lp (args, false, NONE)
    end
			
  fun main (_, args) = let
    val (chatty, n) = getArgs args
    fun doit () = Mandelbrot.mandelbrot n
    val counts = RunSeq.runMicrosec doit
    val _ = if chatty then tellMeAbout counts
            else ()
    in
      counts
    end

end


