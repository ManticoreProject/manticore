(* pipelined-merge.pml
 *
 * COPYRIGHT (c) 2010 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This is a simple divide-and-conquer algorithm which takes two
 * binary trees, T1 and T2, where the keys in each tree are unique and
 * sorted when traversed inorder, and merges them into a sorted tree,
 * T. In terms of number of comparisons, work complexity is O(n + m),
 * and span length is O(lg n + lg m), where n = size(T1) and m =
 * size(T2).
 * 
 * This algorithm was adapted from Pipelining with Futures by Blelloch
 * and Reid Miller (citation below).
 *
 *)

structure Merge =
  struct

    structure F = MultilispFuture

    fun future f = F.future (f, false)

    fun fst (x, y) = x
    fun snd (x, y) = y

    fun ffst f = future (fn () => F.touch (fst (F.touch f)))
    fun fsnd f = future (fn () => F.touch (snd (F.touch f)))

    datatype tree
      = Leaf
      | Node of int * tree * tree

    datatype ftree
      = FLeaf
      | FNode of int * ftree F.future * ftree F.future

    fun ftreeToTree ft =
	(case ft
	  of FLeaf =>
	     Leaf
	   | FNode (k, ft1, ft2) =>
	     Node (k, ftreeToTree (F.touch ft1), ftreeToTree (F.touch ft2)))

    fun treeToFtree t =
	(case t
	  of Leaf =>
	     FLeaf
	   | Node (k, t1, t2) =>
	     FNode (k, future (fn () => treeToFtree t1), 
		       future (fn () => treeToFtree t2)))

  (* split (x, t) *)
  (* Splits tree t into two subtrees (t1, t2) such that t1 contains keys *)
  (* of t that are < x and t2 contains keys of t that are > x. *)
    fun split (x, t) =
	(case t
	  of FLeaf =>
	     (future (fn () => FLeaf), future (fn () => FLeaf))
	   | FNode (k, t1, t2) =>
	     if x = k then
		 (t1, t2) 
	     else if x < k then
		 let
		     val f = future (fn () => split (x, F.touch t1))
		 in
		     (ffst f, future (fn () => FNode (k, fsnd f, t2)))
		 end
	     else
		 let
		     val f = future (fn () => split (x, F.touch t2))
		 in
		     (future (fn () => FNode (k, t1, ffst f)), fsnd f)
		 end)

  (* merge (t1, t2) *)
  (* Merges trees t1 and t2 as specified above. *)
    fun merge (t1, t2) =
	(case (t1, F.touch t2)
	  of (Leaf, t2) => ftreeToTree t2
	   | (t1, FLeaf) => t1
	   | (Node (k, t11, t12), t2) =>
	     let
		 val (t21, t22) = split (k, t2)
	     in
		 Node (| k, merge (t11, t21), merge (t12, t22) |)
	     end)

  end (* Merge *)

structure Main =
  struct
  
    structure M = Merge

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
	    val f = TextIO.openIn "../../../input-data/random-int-list.txt"
	    fun lp acc =
		(case TextIO.inputLine f
		  of NONE => List.rev acc
		   | SOME line => lp (Option.valOf (Int.fromString line) :: acc))
	in
	    lp nil
	end

    fun treeFromList xs =
	(case xs
	  of nil => 
	     M.Leaf
	   | x :: nil =>
	     M.Node (x, M.Leaf, M.Leaf)
	   | xs =>
	     let
		 val n = List.length xs
		 val (xs1, xs2) = (| List.take (xs, n div 2), 
				     List.drop (xs, n div 2) |)
		 val (t1, t2) = (| treeFromList xs1, 
			           treeFromList (List.tl xs2) |)
	     in
		 M.Node (List.hd xs2, t1, t2)
	     end)

    fun treeToList t =
	(case t
	  of M.Leaf => nil
	   | M.Node (k, t1, t2) =>
	     treeToList t1 @ [k] @ treeToList t2)

    fun r () = Rand.inRangeInt (0, 10) mod 10

    fun genRandSortedList n =
	let
	    fun f (i, x) =
		if i >= n then
		    nil
		else
		    let
			val x = r () + x + 1
		    in
			x :: f (i + 1, x)
		    end

	in
	    f (0, r ())
	end

    fun main (_, args) =
	let
	    val n = (case getSizeArg args
		      of NONE => dfltN
		       | SOME n => n)
	    val (x, y) = RunPar.runSilent (fn _ => 
			(treeFromList (genRandSortedList n), 
			 Merge.treeToFtree (treeFromList (genRandSortedList n))))
	    fun doit () = Merge.merge (x, Merge.F.future (fn () => y, false))
		
	in
	    RunPar.run doit
	end

  end

val _ = Main.main (CommandLine.name (), CommandLine.arguments ())

(* 
@Article{	  springerlink:10.1007/s002240000117,
  author	= {Blelloch, G. E. and Reid-Miller, M.},
  affiliation	= {School of Computer Science, Carnegie Mellon University,
		  Pittsburgh, PA 15213-3890, USA blelloch@cs.cmu.edu US},
  title		= {Pipelining with Futures},
  journal	= {Theory of Computing Systems},
  publisher	= {Springer New York},
  issn		= {1432-4350},
  keyword	= {Computer Science},
  pages		= {213-239},
  volume	= {32},
  issue		= {3},
  note		= {10.1007/s002240000117},
  year		= {1999},
}
*)
