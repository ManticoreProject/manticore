(* rope-testss.sml
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

  (* fib : int -> int *)
  fun fib n =
      let fun f (n (* >= 2 *), penpen, pen) =
	        if n=2 then penpen + pen
		else f (n-1, pen, penpen + pen)
      in
	  if n<0 then raise Fail "fib: negative argument"
	  else if n=0 then 0
	  else if n=1 then 1
	  else f (n, 0, 1)
      end

  (* isBalanced : 'a rope -> bool *)
  fun isBalanced r = (R.ropeLen(r) >= fib(R.ropeDepth(r)+2))

  (* range : int * int -> int list *)
  fun range (lo, hi) =
        let fun build (n, acc) = if (n<lo) then acc
				 else build (n-1, n::acc)
	in
	    build (hi, [])
	end
		      
  val r0 = R.fromList (range (1, 15))

  fun test 0 = print (R.toString (r0, Int.toString))
    | test _ = raise Fail "no such test"

end
