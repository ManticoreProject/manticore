(* ropes-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A prototype implementation of ropes in SML.
 * N.B. Not directly used in the compiler.
 *)

functor RangesFn (Arch : ARCH) : RANGES = struct

  structure Ropes = RopesFn (Arch)

  datatype range = Range of int * int * int

  fun todo () = raise Fail "todo"

  val mkRange = Range

  fun buildList (Range (lo, hi, gap)) =
      let fun b (curr, acc) = if curr<lo then raise Fail "grievous error"
	                      else if curr=lo then lo::acc
			      else b (curr-gap, curr::acc)
	  val hi' = if (gap > (hi-lo)) then lo
		    else (hi - ((hi-lo) mod gap))
      in
	  if (lo>hi) then []
	  else b (hi', [])
      end

  val toRope = Ropes.fromList o buildList

  fun toString (Range (lo, hi, gap)) = 
      let val itos = Int.toString
	  val ss =  ["[|", itos lo, " to ", itos hi, " by ", itos gap, "|]"]
      in
	  concat ss
      end

  val ropeToString = Ropes.toString Int.toString

end
