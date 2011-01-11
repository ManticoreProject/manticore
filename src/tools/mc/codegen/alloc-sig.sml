(* alloc-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate code for allocating blocks of memory in the heap.
 *)

signature ALLOC =
  sig
    
    structure MTy : MLRISC_TYPES

  (* select the ith element off the base address *)
    val select : {lhsTy : CFG.ty, mty : CFG.ty, i : int, base : MTy.T.rexp} -> MTy.mlrisc_tree

  (* compute the address of the ith element off the base address *)
    val tupleAddrOf : {mty : CFG.ty, i : int, base : MTy.T.rexp} -> MTy.T.rexp

  (* generate code to allocate a tuple object in the local heap *)
    val genAlloc : {
	    isMut : bool,
	    tys : CFG.ty list,
	    args : MTy.mlrisc_tree list
	  } -> {ptr : MTy.mlrisc_tree, stms : MTy.T.stm list}

  (* generate code to allocate a polymorphic vector in the local heap *)
  (* argument is a pointer to a linked list l of length n *)
  (* vector v is initialized s.t. v[i] := l[i] for 0 <= i < n *)
    val genAllocPolyVec : MTy.T.rexp -> {ptr : MTy.T.reg, stms : MTy.T.stm list}

  (* allocate a list of types in the global heap, and initialize them *)
  (* generate code to allocate a tuple object in the global heap *)
    val genGlobalAlloc : {
	    isMut : bool,
	    tys : CFG.ty list,
	    args : MTy.mlrisc_tree list
	  } -> {ptr : MTy.mlrisc_tree, stms : MTy.T.stm list}

  (* heap limit check.  evaluates to true when the heap contains sufficient
   * space for the given size.
   *)
    val genAllocCheck : word -> {stms : MTy.T.stm list, allocCheck : MTy.T.ccexp}

  (* heap limit check.  evaluates to true when the heap contains sufficient
   * space for the given size.
   *)
    val genAllocNCheck : MTy.T.rexp -> {stms : MTy.T.stm list, allocCheck : MTy.T.ccexp}

  (* global heap limit check.  evaluates to true when the global heap contains sufficient
   * space for the given size.
   *)
    val genGlobalAllocCheck : word -> {stms : MTy.T.stm list, allocCheck : MTy.T.ccexp}

  end (* ALLOC *)
