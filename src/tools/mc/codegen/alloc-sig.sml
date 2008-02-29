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

  (* select the ith element off of a 'base' address *)
    val select : {lhsTy : MTy.T.ty, mty : CFG.ty, i : int, base : MTy.T.rexp} -> MTy.mlrisc_tree

  (* compute the address of the ith element off of a 'base' address *)
    val addrOf : {lhsTy : MTy.T.ty, mty : CFG.ty, i : int, base : MTy.T.rexp} -> MTy.T.rexp

  (* subscript the ith element from the array *)
    val arraySub : {i : MTy.T.rexp, array : MTy.T.rexp} -> MTy.T.rexp

  (* allocate a list of types, and initialize them *)
    val genAlloc : (CFG.ty * MTy.mlrisc_tree) list -> {ptr : MTy.mlrisc_tree, stms : MTy.T.stm list}

  (* allocate a list of types in the global heap, and initialize them *)
    val genGlobalAlloc : (CFG.ty * MTy.mlrisc_tree) list -> {ptr : MTy.mlrisc_tree, stms : MTy.T.stm list}

  (* heap limit check.  evaluates to true when the heap contains sufficient
   * space for the given size.
   *)
    val genAllocCheck : word -> MTy.T.ccexp

  (* global heap limit check.  evaluates to true when the global heap contains sufficient
   * space for the given size.
   *)
    val genGlobalAllocCheck : word -> {stms : MTy.T.stm list, allocCheck : MTy.T.ccexp}

  end (* ALLOC *)
