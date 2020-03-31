
(*
   Sudan function, which is recursive but not primitive recursive:
      https://en.wikipedia.org/wiki/Sudan_function
    It shares this property with the Ackermann function.
*)

structure Params = struct

type t = long

val iterations = 10000000
val input : t * t * t = (2, 2, 2)
val output : t = 15569256417

end



fun sudan (n : Params.t, x : Params.t, y : Params.t) k =
  if n = 0
    then Cont.throw(k, x + y)
  else if y = 0
    then Cont.throw(k, x)
  else let
    val inner = Cont.callec(sudan (n, x, y-1))
  in
    sudan (n-1, inner, inner + y) k
  end





val iterations = Params.iterations
val input = Params.input
val output  = Params.output

(* val _ = Print.printLn (Long.toString (sudan input)) *)

fun lp n = (case n
  of 0 => ()
   | n => (
      if Cont.callec(sudan input) <> output
        then (print "wrong answer!\n"; raise Fail "error!")
        else () ;
      lp (n-1)
      )
  (* end case *))

fun doIt () = (lp iterations; print "ok!\n")

val _ = doIt ()
