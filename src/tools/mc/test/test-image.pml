(* test-image.pml
 *
 * Test the support for PPM images.
 *)

val wid = 256;
val ht = 256;
val img = newImage (wid, ht);

fun setGrey (i, j) = let
      val g = itod(i + j) / 510.0
      in
	updateImage (img, i, j, g, g, g)
      end;

fun lp i = if (i < ht)
      then let
	fun lp' j = if (j < wid)
	      then (setGrey(i, j); lp'(j+1))
	      else ()
	in
	  lp' 0; lp(i+1)
	end
      else ();

val _ = print "start\n";

(lp 0; outputImage(img, "test.ppm"); freeImage img)

