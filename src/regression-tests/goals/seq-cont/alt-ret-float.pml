

fun altRet n ans k =
  if n mod 2 = 0
    then ans
    else Cont.throw (k, ans)


(* fib that alternates between returning normally or using its escape cont *)
fun fib n k = (case n
  of 0 => altRet n 0.0 k
   | 1 => altRet n 1.0 k
   | _ => let
        val left = Cont.callec (fib (n-1))
        val right = Cont.callec (fib (n-2))
      in
        altRet n (left + right) k
      end
  (* end case *))

val f2s = Float.toString
val i2s = Int.toString

val n = 30  (* https://oeis.org/A000045/list *)
val ans = Cont.callec (fib n)

val _ = print ("fib(" ^ i2s n ^ ") = " ^ f2s ans ^ "\n")
