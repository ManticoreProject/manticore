(* amd64-target-spec-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Target-specific information for the AMD64.
 *)

functor AMD64TargetSpecFn (
	val abiName : string
	val osName : string
) :> TARGET_SPEC = struct

  val archName = "amd64"
  val abiName = abiName
  val osName = osName

  val wordSzB = 0w8
  val wordAlignB = 0w8
  val boolSzB = wordSzB

  (* representation of True and False *)
  val trueRep = IntInf.fromInt 1
  val falseRep = IntInf.fromInt 0

  val spillAreaSzB = 0w1024

end (* AMD64TargetSpecFn *)
