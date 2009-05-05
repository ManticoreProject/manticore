(* tree-add.pml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Simple benchmark using parallel tuples. We sum over the leaves of a perfectly
 * balanced ternary tree.
 *)

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

fun mkTree d =
    let
	fun mk d' =
	    if d' >= d then
		LEAF (Rand.inRangeInt (0, 1000))
	    else
		NODE (mk (d'+1), mk (d'+1), mk (d'+1))
    in
	mk 0
    end

fun bench () = let
    val seqCutoff = PrimIO.readInt()
    (* the input is the depth of the tree *)
    val n = PrimIO.readInt()
    val tree = mkTree n
    val (pf, t) = Time.timeToEval(fn () => treeAdd tree)
    in
      Print.print(Long.toString t)
    end

val swp = SwpWorkStealing.workGroup()
val _ = ImplicitThread.runWithGroup(swp, bench)

