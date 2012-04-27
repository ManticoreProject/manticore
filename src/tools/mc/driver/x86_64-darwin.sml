(* x86_64-darwin.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure AMD64TargetSpec = TargetSpecFn (
    val archName = "x86_64"
    val abiName = "??"
    val osName = "darwin"
    structure ABI = RuntimeConstants
    val maxGPRArgs = 8
    val maxFPRArgs = 6
    val maxVPRArgs = 0
    val availRegs = 11)

structure CodeGen = AMD64GenFn (structure Spec = AMD64TargetSpec)

structure Main = MainFn(
    structure Spec = AMD64TargetSpec
    structure CG = CodeGen.Gen)
