(* ntree-add.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel tree add for n-ary trees. The tree add traverses the tree depth first, which causes 
 * the work stealing scheduler to incur a large number of steals.
 *)

datatype tree
  = LEAF of int
  | NODE of tree list

fun plus (x, y) = x + y

fun treeAdd tree =
    (case tree
      of LEAF x => x
       | NODE trees =>
	 let
	     fun walk trees =
		 (case trees
		   of nil => 0
		    | t :: trees => plus (| treeAdd t, walk trees |)
		 (* end case *))
	 in
	     case trees
	      of nil => 0
	       | t :: trees => treeAdd t + walk trees  (* depth first *)
	 end
    (* end case *))

fun mkTree (d, n) =
    let
	fun mk d' =
	    if d' >= d then
		LEAF (Rand.inRangeInt (0, 1000))
	    else
		let 
		    fun mk' i = if i < n then
				    mk (d'+1) :: mk' (i+1)
				else
				    nil
		in
		    NODE (mk' 0)
		end
    in
	mk 0
    end

fun bench () = let
    (* out degree of internal tree nodes *)
    val n = PrimIO.readInt()
    (* depth of the tree *)
    val d = PrimIO.readInt()
    val tree = mkTree (d, n)
    val b = Time.now ()
    val t = treeAdd tree
    val e = Time.now ()
    in
      Print.print(Long.toString (e-b))
    end

val _ = ImplicitThread.runWithGroup(MultiprogrammedWorkStealing.workGroup(), bench)
