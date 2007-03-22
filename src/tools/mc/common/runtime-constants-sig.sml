(* runtime-constants-sig.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Information about the target architecture, operating system, and
 * runtime-system data structures.
 *)

signature RUNTIME_CONSTANTS =
  sig

    val wordSzB : word		 (* number of bytes in a pointer-sized word *)
    val wordAlignB : word	 (* byte alignment of pointers *)
    val boolSzB : word		 (* size of boolean values in bytes *)
    val extendedAlignB : word    (* alignment constraint for extended-precision
				  * floats
				  *)


    val spillAreaSzB : word     (* size of the spill area on the stack *)
    val spillAreaOff : word	(* offset from frame pointer to spill area *)
    val maxObjectSzB : word     (* maximum number of bytes allowable in a *)
				(* heap-allocated object *) 

  (* offsets into the VProc_t structure *)
    val inManticore : IntInf.int
    val atomic : IntInf.int
    val sigPending : IntInf.int
    val actionStk : IntInf.int
    val rdyQHd : IntInf.int
    val rdyQTl : IntInf.int
    val stdArg : IntInf.int 
    val stdPtr : IntInf.int
    val stdCont : IntInf.int
    val stdExnCont : IntInf.int
    val allocPtr : IntInf.int
    val limitPtr : IntInf.int

  (* mask to get address of VProc from allocation pointer *)
    val vpMask : IntInf.int

  end (* RUNTIME_CONSTANTS *)
