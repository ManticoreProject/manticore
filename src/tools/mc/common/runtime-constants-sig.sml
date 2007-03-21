(* runtime-constants-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Information about the target architecture and operating system.
 *)

signature RUNTIME_CONSTANTS = sig

    val wordSzB : word		 (* number of bytes in a pointer-sized word *)
    val wordAlignB : word	 (* byte alignment of pointers *)
    val boolSzB : word		 (* size of boolean values in bytes *)
    val extendedAlignB : word    (* alignment constraint for extended-precision
				  * floats *)


    val spillAreaSzB : word     (* size of the spill area on the stack *)
    val maxObjectSzB : word     (* maximum number of bytes allowable in a *)
				(* heap-allocated object *) 

    (* offsets into the VProc_t structure *)
    val inManticore : int
    val atomic : int
    val sigPending : int
    val allocPtr : int
    val limitPtr : int
    val stdArg : int 
    val stdPtr : int
    val stdCont : int
    val stdExnCont : int
    val actionStk : int

    (* mask to get address of VProc from allocation pointer *)
    val vpMask : Word64.word

    (* common Manticore unboxed values *)
    val falseRep : IntegerLit.integer
    val trueRep : IntegerLit.integer
    val unitRep : IntegerLit.integer
    val nilRep : IntegerLit.integer

end (* RUNTIME_CONSTANTS *)
