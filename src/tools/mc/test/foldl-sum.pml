(* A test of foldl. *)

fun intplus (m:int, n:int) = m+n;

val s = foldl (intplus, 0, 10::10::10::10::10::nil);

print ("The answer is " ^ (itos s) ^ " (expected 50).\n")
