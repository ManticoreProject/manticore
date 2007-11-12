val pc = [| x*x | x in [| 1 to 10 |] |];

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
      "[" ^ (catw (",", build (0, nil))) ^ "]"
  end;

print ("RESULT: " ^ (parrString pc) ^ "\n     (expected 1^2 .. 10^2)\n")

