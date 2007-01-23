(* spill-loc-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Information about the target architecture and operating system.
 *)

signature TARGET_SPEC = sig

    val archName : string
    val abiName : string
    val osName : string

    val wordSzB : int   (* size of machine words in bytes *)
    val boolSzB : int   (* size of boolean values in bytes *)
		  
    (* representation of True and False *)
    val trueRep : IntegerLit.integer
    val falseRep : IntegerLit.integer

end (* TARGET_SPEC *)
