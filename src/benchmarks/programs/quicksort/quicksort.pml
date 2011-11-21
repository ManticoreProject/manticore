(* quicksort.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel quicksort over parallel arrays.
 *)

structure Quicksort = struct

    structure R = IntRope

    fun quicksort (xs:R.rope) = 
	if R.length xs <= 1 then
	    xs
	else
	    let
		val p = R.sub (xs, R.length xs div 2)
		val (lt, eq, gt) = (| R.filter (fn x => x < p) xs, 
				      R.filter (fn x => x = p) xs,
				      R.filter (fn x => x > p) xs |)
		val (l, u) = (| quicksort lt, quicksort gt |)
	    in
		R.cat2 (l, (R.cat2 (eq, u)))
	    end

end

structure Main =
  struct

    val dfltN = 1000000

    fun getSizeArg args =
	(case args
	  of arg1 :: arg2 :: args =>
	     if String.same (arg1, "-size") then Int.fromString arg2
	     else getSizeArg (arg2 :: args)
	   | _ => NONE
	(* end case *))

    fun readFromFile () =
	let
	    val f = TextIO.openIn "../../input-data/random-int-list.txt"
	    fun lp acc =
		(case TextIO.inputLine f
		  of NONE => List.rev acc
		   | SOME line => lp (Option.valOf (Int.fromString line) :: acc))
	in
	    lp nil
	end

    fun main (_, args) =
	let
	    val x = RunPar.runSilent (fn _ => 
		 let
		     val x = IntRope.fromList
				 (case getSizeArg args
				   of NONE => readFromFile ()
				    | SOME n =>
				      List.tabulate (n, fn _ => Rand.inRangeInt (0, 10000)))
		 in
		     x
		 end)
	    fun doit () = Quicksort.quicksort x
		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())

