(* A test of the sequential basis function tabulate. *)

(* Please note the type of tabulate is
 *   (int -> 'a) * int * int -> 'a list 
 *)

fun cube x = x*x*x;

val cubes = tabulate (cube, 0, 10);

fun catw (sep, ss) =
  (case ss
     of nil => ""
      | s::nil => s
      | s::tl => s ^ sep ^ (catw (sep, tl))
    (* end case *));

fun numsToS ns =
  let val s  = catw (",", map (itos, ns))
  in
    "[" ^ s ^ "]"
  end;

print ((numsToS cubes) ^ "\n     (expected cubes for 0,1,...,10)\n")
