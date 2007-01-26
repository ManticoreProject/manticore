(* alloc-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate code for allocating blocks of memory in the heap.
 *)

signature ALLOC = sig
    
    structure MTy : MLRISC_TYPES

    val genAlloc : (CFG.ty * MTy.mlrisc_tree) list -> MTy.T.stm list

end (* ALLOC *)
