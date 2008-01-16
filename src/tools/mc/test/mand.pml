(* mand.pml
 *
 * Sequential version of mandelbrot program.
 *)

val xBase : double = ~2.0;
val yBase : double = 1.25;
val side : double = 2.5;

val sz : int = 32;
val maxCount : int = 1000;
val maxCount' = itof (maxCount-1);

val delta : double = side / (itod(sz-1));

val img = newImage (sz, sz);

fun pixel (i, j) = let
      val c_re = xBase + (delta * itod j)
      val c_im = yBase - (delta * itod i)
      fun loop (cnt, z_re, z_im) = if (cnt < maxCount)
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
      val cnt = loop (0, c_re, c_im)
      val (r, g, b) = if cnt = maxCount
	    then (0.2, 0.0, 0.0)
	    else (*let
	      val w = itof cnt / maxCount'
	      in
		(w, w, 0.25 + w*0.75)
	      end*) (0.1, 0.1, 1.0)
      in
	updateImage3f (img, i, j, r, g, b)
      end;

fun lp i = if (i < sz)
      then let
	fun lp' j = if (j < sz)
	      then (pixel(i, j); lp'(j+1))
	      else ()
	in
	  lp' 0; lp(i+1)
	end
      else ();

(lp 0; outputImage(img, "mand.ppm"); freeImage img)
