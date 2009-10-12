(* pfib.pml
 * 
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Synthetic fib benchmark.
 *)

structure PFib =
  struct

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

end

structure Main =
  struct

    val dfltN = 30
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () = PFib.pFib n
		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
