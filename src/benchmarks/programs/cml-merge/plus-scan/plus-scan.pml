(* plus-scan.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel plus scan over ropes. We use the implementation of plus scan provided
 * by the basis library.
 *)

structure Main =
  struct

    val dfltN = 200000
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () = 
		let
		    val a = Rope.tabP (n, fn _ => (Rand.inRangeInt (0, 100)))
		in 
		    Scan.plusScan a
		end
		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
