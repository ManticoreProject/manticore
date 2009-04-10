(* mandelbrot.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Compute the Mandelbrot set in parallel.
 *)

structure Mandelbrot =
  struct

    val xBase = ~2.0
    val yBase = 1.25
    val side = 2.5

    val maxCount = 1000

    fun mandelbrot N = let
	  fun elt (i, j) = let
	        val delta = side / (Float.fromInt (N-1))
		val c_re = xBase + (delta * Float.fromInt j)
		val c_im = yBase - (delta * Float.fromInt i)
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
	  in
	    [| [| elt (i, j) | j in [| 0 to N-1 |] |] | i in [| 0 to N-1 |] |]
	  end

  end
