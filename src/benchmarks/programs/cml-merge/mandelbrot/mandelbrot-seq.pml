(* mandelbrot-seq.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Sequential version of mandelbrot-set computation. The output goes to out.ppm.
 *)

structure MandelbrotSeq =
  struct

    val xBase = ~2.0
    val yBase = 1.25
    val side = 2.5

    val maxCount = 1000

    fun pix2rgb cnt = 
	if cnt >= maxCount then
	    (0.0, 0.0, 0.0)
	else let
		val w = Float.fromInt cnt / (Float.fromInt (maxCount-1))
	    in
		(w, w, 0.25 + w*0.75)
	    end

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
	  val image = Image.new (N, N)
	  fun output (i, j, (r, g, b)) = Image.update3f (image, i, j, r, g, b);
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
	  in
	    outputImg 0; Image.output("mand.ppm", image); Image.free image;
	    ()
	  end

  end

structure Main =
  struct

    val dfltN = 256
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () = MandelbrotSeq.mandelbrot n
		
	in
	    RunSeq.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
