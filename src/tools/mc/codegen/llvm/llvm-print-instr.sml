(* llvm-print-instr.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Instruction builder used by LLVM Printer
 *)

functor LLVMPrintInstr (structure Spec : TARGET_SPEC) : sig

    val jank : 'a -> 'a
    
  end = struct

  structure C = CFG
  structure CV = CFG.Var
  structure CL = CFG.Label
  structure CT = CFGTy
  structure CF = CFunctions
  structure S = String
  structure Type = AMD64TypesFn (structure Spec = Spec)

  fun jank x = x
     

end
