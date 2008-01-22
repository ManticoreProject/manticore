(* A test of the sequential basis function map. *)

fun pos n = (n > 0);

fun range (lo, hi) =
  let fun build (curr, acc) = 
	    if (curr > hi)
	    then rev acc
	    else build (curr + 1, curr :: acc)
  in
      build (lo, nil)
  end
;

fun b2s b = if b then "true" else "false";

fun printAll bs =
  (case bs
     of nil => ()
      | b::tl => (print (b2s b ^ "\n");
		  printAll tl));

(printAll (map (pos, range (~3, 3)));
 print "(expected F F F F T T T)\n")

