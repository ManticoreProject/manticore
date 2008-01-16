val xBase : real = ~2.0
val yBase : real = 1.25
val side : real = 2.5

val sz : int = 32 (* 1024; *)
val maxCount : int = 255

val delta : real = side / (real(sz-1))

fun pixel (i, j) = let
      val c_re = xBase + (delta * real j)
      val c_im = yBase - (delta * real i)
      fun loop (cnt, z_re, z_im) = if (cnt < maxCount)
	    then let
	      val z_re_sq = z_re * z_re
	      val z_im_sq = z_im * z_im
	      in
		if ((z_re_sq + z_im_sq) > 4.0)
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

fun range (lo, hi) =
 let fun r (curr, acc) =
       if curr < lo
       then acc
       else r (curr-1, curr::acc)
  in
     r (hi, nil)
  end

val img = 
  let val axis = range (0, sz-1)
      val space = map (fn n => map (fn m => (n, m)) axis) axis
  in
      map (map pixel) space
  end

val arrayOfStrings = 
  let (* row2string : int parray -> string *)
      fun row2string r = concat ["[", String.concatWith "," (map Int.toString r), "]"]
  in
      map row2string img
  end

fun prln s = (print (s ^ "\n"))

fun showMe () = app prln arrayOfStrings
