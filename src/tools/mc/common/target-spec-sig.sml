(* spill-loc-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Information about the target architecture and operating system.
 *)

signature TARGET_SPEC =
  sig

    val archName : string
    val abiName : string
    val osName : string

    val wordSzB : word		(* number of bytes in a pointer-sized word *)
    val wordAlignB : word	(* byte alignment of pointers *)
    val boolSzB : word		(* size of boolean values in bytes *)

    val spillAreaSzB : word     (* size of the spill area on the stack *)
    val maxObjectSzB : word     (* maximum number of bytes allowable in a
				 * heap-allocated object *) 
		  
  (* representation of True and False *)
    val trueRep : IntegerLit.integer
    val falseRep : IntegerLit.integer

  end (* TARGET_SPEC *)
