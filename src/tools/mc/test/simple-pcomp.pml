(* Test of a simple parallel comprehension. *)

(*** Utility Functions ***)

fun catw (sep, ss) =
  (case ss
     of nil => ""
      | s::nil => s
      | s::ss  => s ^ sep ^ (catw (sep, ss))
    (* end case *));

fun parrString a =
  let val len = plen a
      fun build (curr, acc) =
        if curr=len
        then rev acc
        else build (curr+1, (itos (a!curr)) :: acc)
  in
      "[" ^ (concatWith (",", build (0, nil))) ^ "]"
  end;

(*** Test Code ***)

val pc = [| x*x | x in [| 1 to 20 |] |];
(* val pc = [| x*x | x in [| 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20 |] |]; *)

val s = parrString pc;

print ("RESULT: " ^ s ^ "\n     (expected 1, 4, 9, ..., 400)\n")

