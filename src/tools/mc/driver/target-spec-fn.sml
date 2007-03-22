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

  ) :> TARGET_SPEC = struct

    val archName = archName
    val abiName = abiName
    val osName = osName

    structure ABI = ABI

  (* representation of True and False *)
    val trueRep : IntegerLit.integer = 3
    val falseRep : IntegerLit.integer = 1

  end (* AMD64TargetSpecFn *)
