(* feedback-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This functor is based on code from the SML/NJ compiler.
 *
 * original version by: Andrew Appel
 * 
 * cleanup by: Matthias Blume
 *    The cleanup involves getting rid of duplicate SCC code (using
 *    the library module GraphSCCFn) and making use of integer set-
 *    and map-modules (from the same library).  The use of SortedList
 *    has been eliminated.
 *)

signature VERTEX =
  sig
    type vertex
    val compare : vertex * vertex -> order
  end

functor FeedbackFn (V : VERTEX) :> sig
  (* Input: A directed graph; that is, a list of vertex-numbers, 
   *        each node with a list of out-edges which indicate other vertices.
   * Output:  A minimum feedback vertex set.
   *
   * Method: branch and bound
   *)
    type vertex = V.vertex
    type node = vertex * vertex list	(* vertex + outgoing edges *)
    type graph = node list

    structure Set : ORD_SET where type Key.ord_key = vertex

    val feedback : graph -> Set.set

  end = struct

  (* NOTE:  By setting MAXDEPTH=infinity, this algorithm will produce
   *        the exact minimum feedback vertex set.  With MAXDEPTH<infinity,
   *        the result will still be a feedback vertex set, but not
   *        always the minimum set.  However, on almost all real programs,
   *        MAXDEPTH=3 will give perfect and efficiently computed results.
   *        Increasing MAXDEPTH will not make the algorithm take longer or
   *        produce better results on "real" programs.
   *)
    val MAXDEPTH = 3

    type vertex = V.vertex
    type node = vertex * vertex list	(* vertex + outgoing edges *)
    type graph = node list

    structure Nd = struct
        type ord_key = vertex
	val compare = V.compare
      end

    structure Set = RedBlackSetFn (Nd)
    fun l2s l = Set.addList (Set.empty, l)
    val s2l = Set.listItems

    structure Map = RedBlackMapFn (Nd)

    structure SCC = GraphSCCFn (Nd)

  (* "normalize" graph by eliminating edges that lead elsewhere *)
    fun normalize g = let
	  val vs = l2s (map #1 g)
	  fun prune (v, e) = (v, Set.intersection (e, vs))
	  in
	    List.map prune g
	  end

    fun scc g = let
	  val roots = map #1 g
	  fun add ((v, e), (sm, fm)) =
		(Map.insert(sm, v, e), Map.insert(fm, v, s2l e))
	  val (setMap, followMap) = foldl add (Map.empty, Map.empty) g
	  fun follow v = Option.valOf (Map.find (followMap, v))
	(* Do the actual scc calculation; for a sanity check we could
	 * match the result against (SCC.SIMPLE root :: _), but we trust
	 * the SCC module and "nontrivial" (below) will take care of
	 * the root node.
	 *)
	  val sccres = SCC.topOrder' { roots = roots, follow = follow }
	(* we already eliminate all trivial (= SIMPLE) components here *)
	  fun toNode v = (v, valOf (Map.find (setMap, v)))
	  fun nontrivial (SCC.SIMPLE _, a) = a
	    | nontrivial (SCC.RECURSIVE l, a) = List.map toNode l :: a
	  val ntcomps = foldr nontrivial [] sccres
	  in
	  (* we finally make each component "self-contained" by pruning
	   * away all edges that lead out of it...
	   *)
	    List.map normalize ntcomps
	  end

    fun feedback graph0 = let
	(* make edges into vertex sets *)
	  val graph = map (fn (v, e) => (v, l2s e)) graph0
	(* any node with an edge to itself MUST be in the minimum feedback
	 * vertex set; remove these "selfnodes" first to make the problem
	 * easier. *)
	  fun hasSelfLoop (v, e) = Set.member (e, v)
	  val (selfnodes, rest) = List.partition hasSelfLoop graph
	(* The following value is part 1 of the final result. *)
	  val selfvertices = l2s (map #1 selfnodes)
	(* with missing nodes, the rest needs to be normalized *)
	  val rest = normalize rest

	(* here is the branch-and-bound algorithm that is used for the rest *)
	  fun feedb (limit, graph, depth) =
		if depth <= 0 then
		  if limit >= length graph then
		    (* approximate! *)
		      SOME (l2s (map #1 graph))
		  else
		    (* Note: the original algorithm would have continued
		     * here when depth < 0; but that seems wrong *)
		      NONE
		else let
		  val comps = scc graph
		  fun g (lim, set, c :: comps) =
			if lim > 0 then
			  (case try (lim, c, depth)
			   of NONE => NONE
			    | SOME vs => g (lim - Set.numItems vs + 1,
					    Set.union (vs, set),
					    comps)
			  (* end case *))
			else NONE
		    | g (lim, set, []) = SOME set
		  in
		    g (limit - length comps + 1, Set.empty, comps)
		  end

	  and try (limit, nodes, depth) = let
	      fun f (best, lim, left, []) = best
		| f (best, lim, left, (node as (x, e)) :: right) =
		  if not (List.null left) andalso Set.numItems e = 1 then
	              (* A node with only one out-edge can't be part of
		       * a unique minimum feedback vertex set, unless they
		       * all have one out-edge. *)
	              f (best, lim, node :: left, right)
		  else let
		    fun prune (n, es) =
			  (n, Set.delete (es, x)
			    handle LibBase.NotFound => es)
		    val reduced = List.map prune (List.revAppend (left, right))
		    in
		      case feedb (lim - 1, reduced, depth - 1)
		       of SOME vs => f (SOME (Set.add (vs, x)), Set.numItems vs, node :: left, right)
			| NONE => f (best, lim, node :: left, right)
		      (* end case *)
		    end
	      in
		f (NONE, Int.min (limit, length nodes), [], nodes)
	      end

	fun bab g = (case feedb (length g, g, MAXDEPTH)
	       of SOME solution => solution
		| NONE => raise Fail "no solution"
	      (* end case *))
	in
	  Set.union (selfvertices, bab rest)
	end

  end
