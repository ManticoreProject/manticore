(* topologies.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This file exports topologies that are commonly used in machine architectures and in algorithm specifications.
 *)

structure Topologies =
  struct

    datatype topologies
      = EMPTY
      | TREE_TOPOLOGY of TreeTopology.topology
      | DEPTH2_TREE_TOPOLOGY of Depth1ForestTopology.topology

  (* local processor of topology *)
    fun self t = (
	  case t
	   of EMPTY => (raise Fail "Topologies: no current topology")
	    | TREE_TOPOLOGY tree => TreeTopology.self tree
	    | DEPTH2_TREE_TOPOLOGY tree => Depth1ForestTopology.self tree
          (* end case *))

  end
