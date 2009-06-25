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
        else build (curr+1, (Int.toString (a!curr)) :: acc)
  in
      "[" ^ (catw (",", build (0, nil))) ^ "]"
  end;

fun max2 (a:int, b:int) = (if (a>b) then a else b);

(*** Test Code ***)

val pc1 = [| max2(x,y) | x in [| 1, 2, 3, 4, 5 |],
                     	 y in [| 5, 4, 3, 2, 1 |] |];
val _ = print ("The length of pc1 is " ^ Int.toString (plen pc1) ^ ".\n");
val s = parrString pc1;
val _ = print ("   pc1: " ^ s ^ "\n        (expected [5,4,3,4,5])\n");

val pc1 = [| max2(x,y) | x in [| 1, 2, 3, 4, 8 |],
                     	 y in [| 5, 4, 3, 2, 1 |] |];
val _ = print ("The length of pc1 is " ^ Int.toString (plen pc1) ^ ".\n");
val s = parrString pc1;
val _ = print ("   pc1: " ^ s ^ "\n        (expected [5,4,3,4,8])\n");

()


