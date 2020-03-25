

fun altRet (n, ans, k) =
  if n mod 2 = 0
    then ans
    else Cont.throw (k, ans)

fun fst (x, _) = x
fun snd (_, x) = x

fun max (a, b) = if a > b then a else b

(* fib that alternates between returning normally or using its escape cont *)
(* FIXME: if you change the `if` below into

    case n
      of 0 => altRet (n, (0, 1), k)
       | 1 => altRet (n, (1, 1), k)
       | _ => let ...

    Then you get the wrong answer, but only when using hybridstacks.
    It's not in the ASM code, since the generated ASM is identical to
    that of segmented stacks. So there must be something wrong in the RTS.
  *)
fun fib n k =
  if n <= 1
    then altRet (n, (n, 1), k)
    else let
        val left = Cont.callec (fib (n-1))
        val right = Cont.callec (fib (n-2))
        val ans = fst left + fst right
        val depth = max (snd left, snd right) + 1
      in
        altRet (n, (ans, depth), k)
      end

val i2s = Int.toString

val n = 30  (* https://oeis.org/A000045/list *)
val res = Cont.callec (fib n)
val ans = fst res
val depth = snd res

val _ = print ("fib(" ^ i2s n ^ ") = " ^ i2s ans ^ ", depth = " ^ i2s depth ^ "\n")
