(* util.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * N.B. Not directly used in the compiler.
 *)

structure Util = struct

  (* fib : int -> int *)
  fun fib n =
      let fun f (n (* >= 2 *), penult, ult) =
	      if n=2 then penult + ult
	      else f (n-1, ult, penult + ult)
      in
	  if n<0 then raise Fail "fib: negative argument"
	  else if n=0 then 0
	  else if n=1 then 1
	  else f (n, 0, 1)
      end
      
end
