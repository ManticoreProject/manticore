(* dt00.pml -- testing the handling of datatypes *)

datatype pt = PT of (float * float);

val zero = PT(0.0, 0.0);

fun add (p1, p2) = let
      val PT(x1, y1) = p1
      val PT(x2, y2) = p2
      in
	PT(x1+x2, y1+y2)
      end;

fun p2s p = (case p
    of PT (x,y) => ftos x^" "^ftos y^"\n"
    (* end case *));

print (p2s (add (PT (1.0, 1.0), PT (2.0, 3.234))))
