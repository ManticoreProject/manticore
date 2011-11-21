(* tree-add.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * We sum over the leaves of a perfectly balanced ternary tree.
 *)

structure TreeAdd =
  struct

    datatype tree
      = LEAF of int
      | NODE of (tree * tree * tree)

    fun treeAdd tree = (case tree
	   of LEAF x => x
	    | NODE (t1, t2, t3) => let
		val (x1, x2, x3) = ( treeAdd t1, treeAdd t2, treeAdd t3 )
		in
		  x1 + x2 + x3
		end
	  (* end case *))
  end

structure Main =
  struct

    structure T = TreeAdd

    val dfltN = 14

    fun mkTree d = let
	  val r = Random.rand (0, 10000)
	  fun mk d' = if d' >= d
		then T.LEAF (Random.randNat r mod 10)
		else T.NODE ( mk (d'+1), mk (d'+1), mk (d'+1) )
	  in
	    mk 0
	  end
	
    fun main (_, args) = let
	  val n = (case args
		    of arg :: _ => Option.getOpt (Int.fromString arg, dfltN)
		     | _ => dfltN)
	  val t = mkTree n
	  fun doit () = T.treeAdd t
	  val res = RunSeq.run doit
	  in
	    (* by checking for a bogus value in the results list, we can hopefully ensure that the
	     * algorithm is execute in its entirety and that key parts are not optimized away by
	     * clever compilers.
	     *)
	    if res < 0 then
		raise Fail "bogus result"
	    else
		();
	    OS.Process.success
	  end

  end
