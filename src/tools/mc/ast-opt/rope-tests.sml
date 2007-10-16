(* rope-tests.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure RopeTests = struct

  structure A = struct
    val cacheLineSizeBytes = 16
    val wordSizeBytes = 4
  end

  structure R = RopesFn (A)

  (* range : int * int -> int list *)
  fun range (lo, hi) =
        let fun build (n, acc) = if (n<lo) then acc
				 else build (n-1, n::acc)
	in
	    build (hi, [])
	end
		      
  val r0 = R.fromList (range (1, 15))

  fun test 0 = print (R.toString Int.toString r0)
    | test _ = raise Fail "no such test"

end
