(* pfib.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthetic fib benchmark.
 *)

fun sFib (n : int) = (
    case n
     of 0 => 0
      | 1 => 1
      | n => let
            val x = sFib (n-1)
            val y = sFib (n-2)
	    in
	      x + y
	    end
    (* end case *))

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
    val (pf, t) = Time.timeToEval(fn () => pFib n)
    val (sf, _) = Time.timeToEval(fn () => sFib n)
    in
    (* sanity test *)
      if pf = sf then () else (raise Fail "incorrect answer"; ());
      Print.printLn("Time elapsed (microseconds): "^Long.toString t)
    end

val _ = bench()
