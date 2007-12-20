(* A test of @. *)
(* Coincidentally, a test of app. *)

fun printBool b = (if b then print "true\n" else print "false\n");

val bs1 = true::true::false::nil;

val bs2 = false::false::true::nil;

val _ = app (printBool, bs1 @ bs2);

print "(expected T T F F F T)\n"
