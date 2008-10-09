(* pfib.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthetic fib benchmark.
 *)

fun pFib (n : int) = (
    case n
     of 0 => 0
      | 1 => 1
      | n => let
            pval x = pFib (n-1)
            val y = pFib (n-2)
	    in
	      x + y
	    end
    (* end case *))

fun bench () = let
    val n = PrimIO.readInt()
    val (_, t) = Time.timeToEval(fn () => pFib n)
    in
      Print.printLn("Time elapsed (microseconds): "^Long.toString t)
    end

val _ = bench()
