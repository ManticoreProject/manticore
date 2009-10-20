(* plus-prefix.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Sequential plus prefix over lists.
 *)

structure PlusPrefix =
  struct

    fun plusPrefix xs =
	let
	    fun lp (xs, p, res) = 
		(case xs
		  of nil => 
		     List.rev res
		   | x :: xs =>
		     let
			 val y = p + x
		     in
			 lp (xs, y, y :: res)
		     end)
	in
	    lp (xs, 0, nil)
	end

  end

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
		    val a = List.tabulate (n, fn _ => (Rand.inRangeInt (0, 100)))
		in 
		    PlusPrefix.plusPrefix a
		end
		
	in
	    RunSeq.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
