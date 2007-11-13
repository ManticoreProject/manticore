(* A test of tabulateStep. *)

fun id (n:int) = n;

val ns = tabulateStep (id, 0, 101, 10);

fun catw (sep, ss) =
  (case ss
     of nil => ""
      | s::nil => s
      | s::ss  => s ^ sep ^ (catw (sep, ss))
    (* end case *));

fun nsString nums =
  let fun build (curr, acc) =
        (case curr
           of nil => rev acc
            | n::ns => build (ns, (itos n) :: acc)
	  (* end case *))
  in
      "[" ^ (catw (",", build (nums, nil))) ^ "]"
  end;

print ("RESULT: " ^ (nsString ns) ^ "\n     (expected 0, 10, ..., 100)\n")

