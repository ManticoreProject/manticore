
signature S =
  sig
    val x : int
  end

structure S =
  struct 
    val x = 34
  end

val x = 34
val z = S.x

(*
fun f x = case x of x :: xs => 2 | nil => 1
val z = print (itos (f nil))
*)
(*
fun f y = (case y of nil => 1 | x :: xs => 2)

fun g y = let
    fun f y = 1
    val z = y
    in f end

val _ = print (ftos 2.4^itos (f nil)^"\n")
*)
