(* alloc-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate code for allocating blocks of memory in the heap.
 *)

signature ALLOC = sig
    
    structure MTy : MLRISC_TYPES

    (* compute the byte offset of the ith entry in tys *)
    val offsetOf : {tys : CFG.ty list, i : int} -> int

    (* select the ith element off of a 'base' address *)
    val select : {lhsTy : MTy.T.ty, mty : CFG.ty, i : int, base : MTy.T.rexp} ->
		 MTy.mlrisc_tree

    (* allocate a list of types, and initialize them *)
    val genAlloc : (CFG.ty * MTy.mlrisc_tree) list -> 
		   {ptr : MTy.mlrisc_tree, stms : MTy.T.stm list}

    (* allocate and initialize a cell for a single value *)
    val genWrap : (CFG.ty * MTy.mlrisc_tree) -> 
		  {ptr : MTy.mlrisc_tree, stms : MTy.T.stm list}

    (* heap limit check.  evaluates to true when the heap contains sufficient
     * space for the given size. *)
    val genAllocCheck : word -> MTy.T.ccexp

end (* ALLOC *)
