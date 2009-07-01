(* mand.pml
 *
 * Sequential version of mandelbrot program.
 *)

val xBase : double = ~1.5;
val yBase : double = 1.0;
val side : double = 2.0;

val sz : int = 1024;
val maxCount : int = 1000;
val maxCount' = Float.toString (maxCount-1);

val delta : double = side / (Double.toString(sz-1));

val img = Image.new (sz, sz);

fun max (a : float, b) = if (a > b) then a else b;
fun min (a : float, b) = if (a < b) then a else b;
fun weight (scale : float, center) = let
      fun sqr x = x*x
      fun f x = 1.0 / (scale * sqr(x - center) + 1.5)
      in
	f
      end;

val red = weight (10.0, 0.75);
val green = weight (10.0, 0.5);
val blue = weight (10.0, 0.25);

fun color cnt = if (cnt = maxCount)
      then (0.0, 0.0, 0.0)
      else let
	val w = Float.toString cnt / maxCount'
	val r = min(1.0, max(0.0, 1.25*(w-0.75) + red w))
	val g = min(1.0, max(0.1, 1.0*(w-0.5) + green w))
	val b = min(1.0, max(0.2, 0.75*(w-0.25) + blue w))
	in
	  (r, g, b)
	end;

fun pixel (i, j) = let
      val c_re = xBase + (delta * Double.toString j)
      val c_im = yBase - (delta * Double.toString i)
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
      val (r, g, b) = color (loop (0, c_re, c_im))
      in
	Image.update3f (img, i, j, r, g, b)
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

val _ = (lp 0; Image.output("mand.ppm", img); Image.free img)
