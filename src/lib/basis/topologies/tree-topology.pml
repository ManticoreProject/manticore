(* tree-topology.pml
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * A tree topology is an abstract processor topology. The topology structure contains a tree of bounded
 * depth and a finger into that tree. The computation moves through this tree using a few given operations.
 *)

structure TreeTopology (* :
  sig

    type location = int

    datatype tree
      = LEAF of location
      | ND of (location * tree * tree)

    datatype ctx
      = TOP | L of (location * ctx * tree) | R of (location * tree * ctx)

    type topology = (tree * ctx)

  (* current location in the tree *)
    val self : topology -> location

  (* movement through the topology *)
    val leftChild : topology -> topology
    val rightChild : topology -> topology
    val parent : topology -> topology
    val root : topology -> topology

  end *) = struct

    type location = int

    datatype tree
      = LEAF of location
      | ND of (location * tree * tree)

    datatype ctx
      = TOP | L of (location * ctx * tree) | R of (location * tree * ctx)

    type topology = (tree * ctx)

    fun self (tree, ctx) = (
	  case tree
	   of LEAF self => self
	    | ND(self, _, _) => self
          (* end case *))

    fun leftChild (tree, ctx) = (
	  case tree
	   of LEAF _ => (raise Fail "at leaf node")
	    | ND(vp, l, r) => (l, L(vp, ctx, r))
          (* end case *))

    fun rightChild (tree, ctx) = (
	  case tree
	   of LEAF _ => (raise Fail "at leaf node")
	    | ND(vp, l, r) => (r, R(vp, l, ctx))
          (* end case *))

    fun parent (tree, ctx) = (
	  case ctx
	   of TOP => (raise Fail "already at root")
	    | L(vp, ctx, r) => (ND(vp, tree, r), ctx)
	    | R(vp, l, ctx) => (ND(vp, l, tree), ctx)
          (* end case *))

    fun root (tree, ctx) = (
	  case ctx
	   of TOP => tree
	    | _ => root(parent(tree, ctx))
          (* end case *))

  end
