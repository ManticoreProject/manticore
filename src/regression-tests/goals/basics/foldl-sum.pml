(* A test of foldl. *)

fun intplus (m:int, n:int) = m+n;

val s = List.foldl intplus 0 (10::10::10::10::10::nil);

val _ = Print.print ("The answer is " ^ (Int.toString s) ^ " (expected 50).\n")
