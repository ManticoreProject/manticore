(* test cases in general, and in particular, typed generated for literals in the case *)

fun sel4 (_, _, _, x) = x
fun sel3 (_, _, x, _) = x

fun break (nil) = ()
  | break (x::xs) = ((case x
  	(* float double int long *)
  	of (_, _, _, 2199023255552) => Print.print ("matched only long " ^ (Long.toString(sel4 x)) ^ "\n")
  	 | (1.7, 1205.543, 135496, 17) => Print.print ("matched all exact\n")
  	 | (_, _, 135496, _) => Print.print ("matched only int " ^ (Int.toString(sel3 x)) ^ "\n")
  	 | (x, 0.0000003, _, _) => Print.print("matched double. " ^ (Float.toString x) ^ "\n")
  	 | (w, x, y, z) => Print.print(
  	 	Float.toString(w) ^ "\t" ^
  	 	Double.toString(x) ^ "\t" ^
  	 	Int.toString(y) ^ "\t" ^
  	 	Long.toString(z) ^ "\t\n"
  	 	)
  	(* esac *)) ; break xs)

val _ = break [
	(1.7, 1205.543, 135496, 17),				(* matched all exact *)
	(1.7, 2.718, 135496, 2199023255552),		(* matched only long 2199023255552 *)
	(0.0, 0.0, 0, 0),							(* 0.000000	0.0000000000	0	0 *)
	(0.00001, 0.0000003, 31337, 17),			(* matched double. 0.000010 *)
	(0.00001, 0.0, 135496, 0)					(* matched only int 135496 *)
]
