(* list-pair.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)


structure ListPair = struct

  (* map : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list *)
    fun map f (xs, ys) = let
      fun loop arg =
       (case arg
	  of (nil, _, acc) => List.rev acc
	   | (_, nil, acc) => List.rev acc
	   | (x::xs, y::ys, acc) => loop (xs, ys, f(x,y)::acc)
          (* end case *))
      in
        loop (xs, ys, nil)
      end

end
