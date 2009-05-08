(* fib-array.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Here is a synthetic benchmark that seeds a parallel array with repeated fib(i) 
 * computations. Supposing that the optimizer does not hoist this computation outside
 * the parallel-array map, we should expect a near linear speedup of the computation.
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

fun bench () = let
    val i = PrimIO.readInt()
    val n = PrimIO.readInt()
    val a = tabP (n, fn _ => i)
    val (a', t) = Time.timeToEval(fn () => [| sFib i | i in a |])
    in
      Print.print(Long.toString t);
      ()
    end

val workStealing = MultiprogrammedWorkStealing.workGroup()
val _ = ImplicitThread.runWithGroup(workStealing, bench)
