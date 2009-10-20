(* tree-add.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Simple benchmark using parallel tuples. We sum over the leaves of a perfectly
 * balanced ternary tree.
 *)

structure TreeAdd =
  struct

    datatype tree
      = LEAF of int
      | NODE of (tree * tree * tree)

    fun treeAdd tree =
	(case tree
	  of LEAF x => x
	   | NODE (t1, t2, t3) =>
	     let
		 val (x1, x2, x3) = (| treeAdd t1, treeAdd t2, treeAdd t3 |)
	     in
		 x1 + x2 + x3
	     end)
  end

structure Main =
  struct

    structure T = TreeAdd

    val dfltN = 10

    fun mkTree d =
	let
	    fun mk d' =
		if d' >= d then
		    T.LEAF (Rand.inRangeInt (0, 1000))
		else
		    T.NODE (mk (d'+1), mk (d'+1), mk (d'+1))
	in
	    mk 0
	end
	
    fun main (_, args) =
	let
	    val n = (case args
		      of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		       | _ => dfltN)
	    fun doit () = T.treeAdd (mkTree n)
		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
