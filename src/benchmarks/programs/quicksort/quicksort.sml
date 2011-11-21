(* quicksort.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel quicksort over parallel arrays.
 *)

structure Rope = RopeImplFn (
		   structure Seq = VectorSeq
		   structure RT = SimpleRuntime
		   val C = 2.0
		)

structure Quicksort = struct

    structure R = Rope

    fun quicksort xs =
	if R.length xs <= 1 then
	    xs
	else
	    let
		val p = Rope.sub (xs, R.length xs div 2)
		val (lt, eq, gt) = ( R.filter (fn x => x < p) xs, 
				      R.filter (fn x => x = p) xs,
				      R.filter (fn x => x > p) xs )
		val (l, u) = ( quicksort lt, quicksort gt )
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
	     if String.compare (arg1, "-size") = EQUAL then Int.fromString arg2
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

    val rand = Random.rand (0, 10000)

    fun main (_, args) =
	let
	    val x = 
		 let
		     val x = Rope.fromList
				 (case getSizeArg args
				   of NONE => readFromFile ()
				    | SOME n =>
				      List.tabulate (n, fn _ => Random.randNat rand))
		 in
		     (* the map below has the effect of distributing the parallel array across per-vproc nurseries, thereby
		      * distributing subsequent GC load
		      *)
		     Rope.map (fn y => y+1) x
		 end
	    fun doit () = Quicksort.quicksort x
		
	in
	    RunSeq.run doit
	end

  end
