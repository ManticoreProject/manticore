(* arch-types-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Converts CFG types into raw sizes in the architecture.
 *)

signature ARCH_TYPES = sig

    (* number of bytes needed to store an aligned CFG object *)
    val alignedTySzB : CFG.ty -> int

    (* number of bits needed to contain a CFG object (used for MLTREE types)*)
    val szOf : CFG.ty -> int

end (* ARCH_TYPES *)
