(* Test of a simple parallel comprehension. *)

(*** Utility Functions ***)

fun parrString a =
  let val len = plen a
      fun build (curr, acc) =
        if curr=len
        then rev acc
        else build (curr+1, (Int.toString (a!curr)) :: acc)
  in
      "[" ^ (concatWith (",", build (0, nil))) ^ "]"
  end;

(*** Test Code ***)

fun sqr(x:int) = x*x;

val pc = [| sqr(x) | x in [| 1 to 20 |] |];

val s = parrString pc;

val _ = print ("RESULT: " ^ s ^ "\n     (expected 1, 4, 9, ..., 400)\n")

