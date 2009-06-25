(* A test of @. *)
(* Coincidentally, a test of app. *)

fun printBool b = (if b then Print.print "true\n" else Print.print "false\n");

val bs1 = true::true::false::nil;

val bs2 = false::false::true::nil;

val _ = List.app printBool (bs1 @ bs2);

val _ = Print.print "(expected T T F F F T)\n"
