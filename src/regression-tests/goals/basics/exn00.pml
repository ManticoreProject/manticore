(* exn00.pml *)

exception Foo of int;

(*fun f g = (g(); 42) handle _ => 12;*)

fun g' () = raise Foo 17;

val _ = (*Print.print(Int.toString(f g') ^ "\n")*) 0

