(* A test of the sequential basis function app. *)

fun printBool b = (if b then Print.print "true\n" else Print.print "false\n");

val _ = List.app printBool (true::false::false::true::nil);

val _ = Print.print "(expected true, false, false, true)\n"
