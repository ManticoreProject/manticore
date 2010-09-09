exception Bad;
exception EvenWorse of string;

fun help b = if b then raise Bad else raise EvenWorse "help!";

fun itsOK () = (help false; "OK") handle Bad => "Bad" | EvenWorse s => s;

val _ = itsOK ()

