(* fcms-ex.sml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Example of using the "first-class" aspect of schedules. We parameterize the parallel treeAdd
 * function over schedules.
 *)

functor FCMSExFn (

    structure F : FCMS

  ) = struct

    datatype tree
      = LEAF of int
      | ND2 of (tree * tree)
      | ND3 of (tree * tree * tree)

    fun treeAdd sched2 sched3 tree = let
	  fun treeAdd' tree = (
		case tree
		 of LEAF x => x
		  | ND2 (t1, t2) => let
		      val f1 = F.future(fn () => treeAdd' t1)
		      val f2 = F.future(fn () => treeAdd' t2)
		      in
			F.sched (F.future(fn () => F.touch f1 + F.touch f2)) (sched2 f1 f2)
		      end
		  | ND3 (t1, t2, t3) => let
		      val f1 = F.future(fn () => treeAdd' t1)
		      val f2 = F.future(fn () => treeAdd' t2)
		      val f3 = F.future(fn () => treeAdd' t3)
		      in
			F.sched (F.future(fn () => F.touch f1 + F.touch f2 + F.touch f3)) (sched3 f1 f2 f3)
		      end
		(* end case *))
          in
	    treeAdd' tree
	  end

  (* schedules for two-node trees *)
    fun seq2 f1 f2 = F.seq (F.e f1) (F.e f2)
    fun par2 f1 f2 = F.par (F.d f1) (F.d f2)

  (* schedules for three-node trees *)
  (* evaluate the middle subtree after either the left or right subtree has completed *)
    fun middleNodeSecondOrLast f1 f2 f3 = F.par (F.seq (F.e f1) (F.e f2)) (F.seq (F.e f3) (F.e f2))
  (* evaluate all three subtrees in parallel *)
    fun par3 f1 f2 f3 = F.par (F.par (F.d f1) (F.d f2)) (F.d f3)

  (* different schedules should not affect the result *)
    fun check1 t = treeAdd seq2 par3 t = treeAdd par2 middleNodeSecondOrLast t
    fun check2 t = treeAdd par2 par3 t = treeAdd seq2 par3 t

    val t = ND2(LEAF 1, LEAF 2)

    fun report b = if b then () else raise Fail "check failed"

    val _ = report(check1 t)
    val _ = report(check2 t)

  end
