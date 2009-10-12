(* fib-array.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Here is a synthetic benchmark that seeds a parallel array with repeated fib(i) 
 * computations. Supposing that the optimizer does not hoist this computation outside
 * the parallel-array map, we should expect a near linear speedup of the computation.
 *)

structure Main =
  struct

    val dfltN = 1000
    val dfltI = 20

    fun sFib (n : int) = (case n
	 of 0 => 0
	  | 1 => 1
	  | n => let
		val x = sFib (n-1)
		val y = sFib (n-2)
		in
		  x + y
		end
	(* end case *))

    fun main (_, args) =
	let
	    val (n, i) = (case args
		      of arg1 :: arg2 :: _ => (Option.getOpt (Int.fromString arg1, dfltN),
					       Option.getOpt (Int.fromString arg2, dfltI))
		       | _ => (dfltN, dfltI))
	    fun doit () = tabP (n, fn _ => sFib i)
		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
