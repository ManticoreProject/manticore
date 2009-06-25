(* A test of foldr. *)

fun intplus (m:int, n:int) = m+n;

val s = foldr (intplus, 0, 10::10::10::10::10::nil);

val _ = Print.print ("The answer is " ^ (Int.toString s) ^ " (expected 50).\n")
