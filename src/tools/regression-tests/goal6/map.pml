(* A test of the sequential basis function map. *)

fun pos n = (n > 0);

fun fromto (lo, hi) =
  let fun build (curr, acc) = 
	    if (curr > hi)
	    then rev acc
	    else build (curr + 1, curr :: acc)
  in
      build (lo, nil)
  end

fun b2s b = if b then "true" else "false"

fun printAll bs =
  (case bs
     of nil => print "\n"
      | b::tl => (print (b2s b ^ " ");
		  printAll tl))

val _ = printAll (map (pos, fromto (~3, 3)))

