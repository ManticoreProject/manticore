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

    (* allocate a list of types, and initialize them *)
    val genAlloc : (CFG.ty * MTy.mlrisc_tree) list -> MTy.T.stm list

end (* ALLOC *)
