val xBase : double = ~2.0;
val yBase : double = 1.25;
val side : double = 2.5;

val sz : int = 16; (* 1024; *)
val maxCount : int = 255;

val delta : double = side / (itod sz);

fun pixel (i, j) = let
      val c_re = xBase + (delta * itod j)
      val c_im = yBase - (delta * itod i)
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
      end;

(* TRY COMMENTING OUT THIS DEF OF PIXEL AND OBSERVE THE ERROR... *)
fun pixel (i,j) = i;

val img = [| [| pixel(i, j) | j in [| 0 to sz-1 |] |] | i in [| 0 to sz-1 |] |];

(* catw : string * string parray -> string *)
fun catw (sep, strPar) =
  let val n = plen strPar
      fun go (curr, acc) =
        if curr >= n then 
          acc
        else if curr = (n-1) then
          acc ^ (strPar ! curr)
        else
          go (curr+1, acc ^ (strPar!curr) ^ sep)
  in
      go (0, "")
  end;

val arrayOfStrings = 
  let (* row2string : int parray -> string *)
      fun row2string r = "[" ^ catw (",", [| itos n | n in r |]) ^ "]";
  in
      [| row2string r | r in img |]
  end;

fun prln s = (print (s ^ "\n"));

parrayApp (prln, arrayOfStrings)
