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

    structure ABI : RUNTIME_CONSTANTS

  (* representation of True and False *)
    val trueRep : IntegerLit.integer
    val falseRep : IntegerLit.integer

  (* information about registers available for argument passing; the
   * GPR count must be at least four to support the standard calling
   * convention (clos, arg, ret, and exh).
   *)
    val maxGPRArgs : int	(* max number of general-purpose-register args *)
    val maxFPRArgs : int	(* max number of floating-point-register args *)
    val maxVPRArgs : int	(* max number of vector-register args *)

  end (* TARGET_SPEC *)

