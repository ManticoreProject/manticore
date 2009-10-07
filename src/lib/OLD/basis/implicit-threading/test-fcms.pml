(* test-fcms.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Simple treeAdd test of first-class monadic schedules.
 *)

structure F = FCMS

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
fun seq3 f1 f2 f3 = F.seq (F.e f1) (F.seq (F.e f2) (F.e f3))
(* evaluate all three subtrees in parallel *)
fun par3 f1 f2 f3 = F.par (F.par (F.d f1) (F.d f2)) (F.d f3)

fun report b = if b then () else raise Fail "check failed"

fun genTree depth = 
      if depth <= 0 then LEAF (Rand.inRangeInt(0, 1024))
      else if Rand.inRangeInt(0, 2) = 1
         then ND2(genTree(depth-1), genTree(depth-1))
      else ND3(genTree(depth-1), genTree(depth-1), genTree(depth-1))

fun seqTreeAdd tree = (
      case tree
       of LEAF x => x
	| ND2 (t1, t2) => seqTreeAdd t1 + seqTreeAdd t2
	| ND3 (t1, t2, t3) => seqTreeAdd t1 + seqTreeAdd t2 + seqTreeAdd t3
      (* end case *))

val globalBFS = GlobalBFSScheduler.workGroup()

val () = ImplicitThread.runWithGroup(globalBFS, fn () => let

val t = genTree 6
(* different schedules should not affect the result *)
fun check1 t = report(treeAdd seq2 par3 t = seqTreeAdd t)
fun check2 t = report(treeAdd par2 par3 t = seqTreeAdd t)
fun check3 t = report(treeAdd par2 middleNodeSecondOrLast t  = seqTreeAdd t)
fun check4 t = report(treeAdd seq2 par3 t = seqTreeAdd t)
fun check5 t = report(treeAdd seq2 seq3 t = seqTreeAdd t)
val () = check1 t
val () = check2 t
val () = check3 t
val () = check4 t

in () end)
