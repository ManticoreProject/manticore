datatype 'a wrapped
  = Wrapped of 'a

(* this program was written to confirm that PINEAPPLE has type float in the AST...it does *)

fun foo ((Wrapped PINEAPPLE) : float wrapped) = 0

val zero = foo (Wrapped 0.0)

val _ = Print.printLn "done."
