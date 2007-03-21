(* target-spec-sig.sml
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

    structure C : RUNTIME_CONSTANTS

  end (* TARGET_SPEC *)
