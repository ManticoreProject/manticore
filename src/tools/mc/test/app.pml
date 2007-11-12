(* A test of the sequential basis function app. *)

fun printBool b = (if b then print "true\n" else print "false\n");

val _ = app (printBool, true::false::false::true::nil);

print "(expected true, false, false, true)\n"
