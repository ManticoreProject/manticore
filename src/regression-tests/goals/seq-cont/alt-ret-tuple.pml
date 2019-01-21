

fun altRet (n, ans, k) =
  if n mod 2 = 0
    then ans
    else Cont.throw (k, ans)

fun fst (x, _) = x
fun snd (_, x) = x

fun max (a, b) = if a > b then a else b

(* fib that alternates between returning normally or using its escape cont *)
fun fib n k = (case n
  of 0 => altRet (n, (0, 1), k)
   | 1 => altRet (n, (1, 1), k)
   | _ => let
        val left = Cont.callec (fib (n-1))
        val right = Cont.callec (fib (n-2))
        val ans = fst left + fst right
        val depth = max (snd left, snd right) + 1
      in
        altRet (n, (ans, depth), k)
      end
  (* end case *))

val i2s = Int.toString

val n = 30  (* https://oeis.org/A000045/list *)
val res = Cont.callec (fib n)
val ans = fst res
val depth = snd res

val _ = print ("fib(" ^ i2s n ^ ") = " ^ i2s ans ^ ", depth = " ^ i2s depth ^ "\n")
