(*
signature S =
  sig
    val x : int
  end

structure S : S =
  struct 
    val x = 34
  end

val x = 34
val z = S.x
*)

type t = int

val _ = print (ftos 2.4^"\n")
