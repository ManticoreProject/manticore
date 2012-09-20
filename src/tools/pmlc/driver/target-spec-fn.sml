(* target-spec-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor TargetSpecFn (

    val archName : string
    val abiName : string
    val osName : string
    structure ABI : RUNTIME_CONSTANTS
    val maxGPRArgs : int
    val maxFPRArgs : int
    val maxVPRArgs : int
    val availRegs : int

  ) :> TARGET_SPEC = struct

    val archName = archName
    val abiName = abiName
    val osName = osName

    structure ABI = ABI

  (* representation of True and False *)
    val trueRep : IntegerLit.integer = 3
    val falseRep : IntegerLit.integer = 1

  (* information about registers available for argument passing; the
   * GPR count must be at least four to support the standard calling
   * convention (clos, arg, ret, and exh).
   *)
    val maxGPRArgs = maxGPRArgs
    val maxFPRArgs = maxFPRArgs
    val maxVPRArgs = maxVPRArgs
    val availRegs = availRegs

  end (* TargetSpecFn *)
