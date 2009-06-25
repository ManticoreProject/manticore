(* A test of the sequential basis function foldr. *)

fun range (lo, hi) =
  let fun build (curr, acc) = 
	    if (curr > hi)
	    then List.rev acc
	    else build (curr + 1, curr :: acc)
  in
      build (lo, nil)
  end
;

val r = range (0, 100);

fun mx (a:int, b:int) = if (a>b) then a else b;

val IHopeIts100 =
  (case r
     of nil => fail "bug"
      | n::ns => List.foldr mx n ns
  (* end case *));

fun main _ = Print.print ((Int.toString IHopeIts100) ^ " (expected 100).\n")

