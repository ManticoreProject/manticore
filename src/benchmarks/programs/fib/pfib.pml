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
            val (x, y) = (| pFib (n-1), pFib (n-2) |)
	    in
	      x + y
	    end
    (* end case *))

fun bench () = let
  (* throw away the sequential cutoff here... we need to read it for compatibility with benchmarking script *)
    val seqCutoff = PrimIO.readInt()
    val n = PrimIO.readInt()
    val (pf, t) = Time.timeToEval(fn () => pFib n)
    val (sf, _) = Time.timeToEval(fn () => sFib n)
    in
    (* sanity test *)
      if pf = sf then () else (raise Fail "incorrect answer"; ());
      Print.print(Long.toString t)
    end

val _ = bench()
(*val workStealing = MultiprogrammedWorkStealing.workGroup()
val _ = ImplicitThread.runWithGroup(workStealing, bench)
*)
