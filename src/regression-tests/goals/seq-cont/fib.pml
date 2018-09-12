
(* never returns normally *)

fun contFib n k = ((case n
      of 0 => Cont.throw (k, 0)
       | 1 => Cont.throw (k, 1)
       | n => let
                 val n1 = Cont.callec (contFib (n-1))
                 val n2 = Cont.callec (contFib (n-2))
              in
                Cont.throw (k, n1 + n2)
              end
      (* end case *))
  ; 500)  (* 500 is not a fibonacci number. *)


val i2s = Int.toString

val n = 15
val ans = Cont.callec (contFib n)

val _ = print ("fib(" ^ i2s n ^ ") = " ^ i2s ans ^ "\n")
