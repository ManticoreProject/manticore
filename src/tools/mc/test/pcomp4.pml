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
      "[|" ^ (catw (",", build (0, nil))) ^ "|]"
  end;

fun listmax (ns:int list) =
  let fun m (ns:int list, mx:int) =
            (case ns
               of nil => mx
                | (n::ns) => m (ns, if (n>mx) then n else mx))
  in
    case ns
      of n::ns => m (ns, n)
       | nil => ~111111 (* error *)
  end;

(*** Test Code ***)

val pc1 = [| listmax(x::y::z::w::nil) |  x in [| 1, 2, 3, 4, 5 |],
                                         y in [| 5, 4, 3, 2, 1 |],
                                         z in [| 0, 5, 4, 2, 0 |],
                                         w in [| 9, 8, 0, 0, 0 |] |];

val _ = print ("The length of pc1 is " ^ Int.toString (plen pc1) ^ ".\n");
val s = parrString pc1;
val _ = print ("pc1: " ^ s ^ "\n(expected [|9,8,4,4,5|])\n");

()


