(* depth1-forest.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A depth-one forest topology is an abstract processor topology. The topology structure contains a forest
 * of depth-1 trees and a finger into one of those trees. The computation moves through this forest using
 * a few given operations.
 *)

structure Depth1ForestTopology (* :
  sig

    type topology = (
	   int *                            (* self *)
	   int *                            (* sibling *)
	   (int * int) list                 (* others *)
         )

  (* number of processors in the forest *)
    val nVProcs : topology -> int

  (* current location in the tree *)
    val self : topology -> int

  (* move through the forest *)
    val sibling : topology -> topology
    val next : topology -> topology

  end *) = struct

    type topology = (
	   int *                            (* self *)
	   int *                            (* sibling *)
	   (int * int) list                 (* others *)
         )

    fun nVProcs (_, _, others) = 2 + List.length others

    fun self (self, _, _) = self

    fun sibling (self, sibling, others) = (sibling, self, others)

    fun next (self, sibling, others) = (
	  case others
	   of nil => (self, sibling, others)
	    | (self', sibling') :: others' => (self', sibling', others' @ (self, sibling) :: nil)
          (* end case *))

  end
