(* exn01.pml *)

exception Foo of int;

fun f g = (g(); 42) handle Foo n => n;

val _ = ()

