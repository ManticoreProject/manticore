structure Ropes (* : ROPES *) =
  struct

    structure S = ListSeq
    type 'a seq = 'a S.seq

  (* ***** UTILITIES ***** *)

  (* itos : int -> string *)
    val itos = Int.toString

  (* log : real -> (real -> real) *)
    fun log base x = Math.ln x / Math.ln base

  (* fib : int -> int *)
  (* Compute the nth Fibonacci number, where *)
  (*   fib 0 is 0, fib 1 is 1, fib 2 is 1, etc. *)
  (* Returns 0 for negative args, so be careful. *)
    fun fib n = let
      fun ff args =
       (case args
	  of (0, u, p) => u
	   | (n, u, p) => ff (n-1, u+p, u)
          (* end case *))
      in
        if n < 1 then 0
	else if n = 0 then 1
	else ff (n, 0, 1)
      end


  end
