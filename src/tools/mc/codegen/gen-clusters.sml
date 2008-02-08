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

  fun addEdges (M.FUNC {lab, exit, ...}, edgeMap) =
      let (* add an undirected edge: lab <-> l *)
	  fun addEdge (l, edgeMap) =
	      (case (LM.find (edgeMap, lab), LM.find (edgeMap, l))
		of (SOME labLs, SOME lLs) =>
		   let val edgeMap = LM.insert (edgeMap, lab, LS.add (labLs, l))
		   in 
		       LM.insert (edgeMap, l, LS.add (lLs, lab))
		   end
		 | _ => raise Fail "addEdge"
	      (* esac *))
	  (* get the jump labels of a transfer *)
	  val labs = CFGUtil.labelsOfXfer exit
      in
	  foldl addEdge edgeMap labs
      end (* addEdges *)

  (* clusters takes a list of functions, and returns the clusters of those
   * functions.  The function builds clusters by first creating the undirected
   * graph G=(V=labMap, E=edgeMap), where labMap contains the labels of all CFG
   * functions, and maps them to their CFG representation.  Since the graph is
   * undirected, I use the SCC library to compute the connected components of
   * G. Although this technique is inefficient, it's simpler than coding
   * the CCS manually or with Union Find, and probably won't make a difference.
   *)
  fun clusters code =
      let fun insLab (func as M.FUNC {lab, ...}, (edgeMap, labMap)) =
	      (LM.insert (edgeMap, lab, LS.empty), LM.insert (labMap, lab, func))
	  (* initialize the edgeMap with empty edges, and map each function label
	   * back to its function in the labMap *)
	  val (edgeMap, labMap) = foldl insLab (LM.empty, LM.empty) code
	  fun getFunc l = (case LM.find (labMap, l)
			    of SOME f => f
			     | NONE => raise Fail "getFunc"
			  (* esac *))
	  (* build the graph of jump edges *)
	  val edgeMap = foldl addEdges edgeMap code
	  fun follow l = (case LM.find (edgeMap, l)
			   of SOME lSet => LS.listItems lSet
			    | _ => raise Fail "follow"
			 (* esac *))
	  val sccs = SCC.topOrder' {roots=LM.listKeys labMap, follow=follow}
	  fun toCluster (SCC.SIMPLE l) = [getFunc l]
	    | toCluster (SCC.RECURSIVE ls) = map getFunc ls
      in
	  map toCluster sccs
      end (* clusters *)

end (* GenClusters *)
