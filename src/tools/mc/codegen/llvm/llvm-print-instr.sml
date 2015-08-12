(* llvm-builder.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Instruction builder used by LLVM Printer
 *)

functor LLVMBuilder (structure Spec : TARGET_SPEC) : sig

    type instr
    type var
    type ty

    val asInstr : LLVMVar.var -> instr

    
    
  end = struct

  type ty = LLVMType.ty
  type var = LLVMVar.var

  fun asInstr x = x
     

end
