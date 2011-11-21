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

    (* number of bits to represent a CFG type (the MLRISC type) *)
    val szOf : CFG.ty -> int

    (* number of bits to represent a the i^th position of a CFG type (the MLRISC type) *)
    val szOfIx : (CFG.ty * int) -> int

end (* ARCH_TYPES *)
