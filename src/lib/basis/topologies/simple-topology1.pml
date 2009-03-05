(* simple-topology1.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A simple topology that can be used to model multicore processors with shared caches.
 *)

structure SimpleTopology1 (* :
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
