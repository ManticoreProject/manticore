(* exn00.pml *)

exception Foo of int;

(*fun f g = (g(); 42) handle _ => 12;*)

fun g' () = raise Foo 17;

(*print(itos(f g') ^ "\n")*) 0

