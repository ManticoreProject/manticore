val xBase : double = ~2.0;
val yBase : double = 1.25;
val side : double = 2.5;

val sz : int = 1024;
val maxCount : int = 1000;
val maxCount' = itof (maxCount-1);

val delta : double = side / (itod (sz-1));

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
      in
	loop (0, c_re, c_im)
      end;

fun pix2rgb cnt = if cnt = maxCount
    then (0.2, 0.0, 0.0)
    else let
	      val w = itof cnt / maxCount'
	      in
		(w, w, 0.25 + w*0.75)
	      end;

fun output (i, j, (r, g, b)) = updateImage3f (img, i, j, r, g, b);

val data = 
  let val axis = [| n | n in [| 0 to sz-1 |] |] (* FIXME this eta-equiv trick should not be necessary *)
                                                (* should just be [| 0 to sz-1 |] *)
  in
      [| [| pix2rgb (pixel(i, j)) | j in axis |] | i in axis |]
  end;

fun outputImg i = if i < sz
        then let
          fun loop j = if j < sz
              then (
                   output (i, j, (data!i)!j);
                   loop (j+1)
                )
              else outputImg (i+1)
          in
             loop 0
          end
        else ();

(outputImg 0; outputImage(img, "mand.ppm"); freeImage img)
