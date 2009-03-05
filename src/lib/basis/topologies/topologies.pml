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
      | SIMPLE_TOPOLOGY1 of SimpleTopology1.topology

  (* local processor of topology *)
    fun self t = (
	  case t
	   of EMPTY => (raise Fail "Topologies: no current topology")
	    | TREE_TOPOLOGY tree => TreeTopology.self tree
	    | SIMPLE_TOPOLOGY1 top => SimpleTopology1.self top
          (* end case *))

  end
