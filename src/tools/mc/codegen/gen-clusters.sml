(* gen-clusters.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Clusters are connected blocks in the CFG representation.  This module
 * computes the clusters of a CFG module.
 *)

structure GenClusters :> sig
    type cluster = CFG.func list
    (* return the clusters for all functions in a module *)
    val clusters : CFG.func list -> cluster list
end = struct

  structure M = CFG
  structure LM = CFG.Label.Map
  structure LS = CFG.Label.Set
  structure SCC = GraphSCCFn (
	type ord_key = M.Label.var
	val compare = M.Label.compare )

  type cluster = CFG.func list

  fun addEdges (M.FUNC {lab, start as M.BLK{exit,...}, body, ...}, edgeMap) = let
      (* add an undirected edge: lab <-> l *)
      fun addEdge (l, edgeMap) = (case (LM.find (edgeMap, lab), LM.find (edgeMap, l))
	  of (SOME labLs, SOME lLs) => let
              val edgeMap = LM.insert (edgeMap, lab, LS.add (labLs, l))
	      in 
		 LM.insert (edgeMap, l, LS.add (lLs, lab))
	     end
	   | _ => raise Fail "addEdge")
      (* get the jump labels of a transfer *)
      val exits = [exit]
      val exits = List.foldl (fn (M.BLK{exit,...}, ls) => exit::ls) exits body
      val labs = List.foldl (fn (exit, labs) => (CFGUtil.labelsOfXfer exit) @ labs) [] exits
      in
	  List.foldl addEdge edgeMap labs
      end

  (* clusters takes a list of functions, and returns the clusters of those
   * functions.  The function builds clusters by first creating the undirected
   * graph G=(V=labMap, E=edgeMap), where labMap contains the labels of all CFG
   * functions, and maps them to their CFG representation.  Since the graph is
   * undirected, I use the SCC library to compute the connected components of
   * G. Although this technique is inefficient, it's simpler than coding
   * the CCS manually or with Union Find, and probably won't make a difference.
   *)
  fun clusters code = let
      fun doFunc (func as M.FUNC {start, body, ...}, (edgeMap, labMap)) = let
          fun insLab (block as M.BLK {lab, ...}, (edgeMap, labMap)) =
	      (LM.insert (edgeMap, lab, LS.empty), LM.insert (labMap, lab, func))
      in
          insLab (start, List.foldl insLab (edgeMap, labMap) body)
      end
      (* initialize the edgeMap with empty edges, and map each function label
       * back to its function in the labMap *)
      val (edgeMap, labMap) = List.foldl doFunc (LM.empty, LM.empty) code
      fun getFunc l = Option.valOf (LM.find (labMap, l))
      (* build the graph of jump edges *)
      val edgeMap = List.foldl addEdges edgeMap code
      fun follow l = LS.listItems (Option.valOf (LM.find (edgeMap, l)))
      val sccs = SCC.topOrder' {roots=LM.listKeys labMap, follow=follow}
      fun toCluster (SCC.SIMPLE l) = [getFunc l]
	| toCluster (SCC.RECURSIVE ls) = List.map getFunc ls
      val clustersWithDups = List.map toCluster sccs
      fun uniqueify funs = LM.listItems (List.foldl (fn (f as CFG.FUNC{lab,...},lm) => LM.insert (lm, lab, f)) LM.empty funs)
      in
          List.map uniqueify clustersWithDups
      end
end
