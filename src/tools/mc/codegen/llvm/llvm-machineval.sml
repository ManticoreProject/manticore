(* llvm-statepoint.sml
 * 
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Lightweight utilities for managing the implicit machine values found 
 * in the CFG representation
 *)

structure LLVMMachineVal = struct
  
  datatype machineVal 
    = MV_Alloc
    | MV_Vproc
    
  fun machineInfo mv = (case mv
    of MV_Alloc => (0, "allocPtr", LLVMTy.allocPtrTy)
     | MV_Vproc => (1, "vprocPtr", LLVMTy.vprocTy)
    (* end case *))
    
  fun machineValIdx mv = #1(machineInfo mv)
  fun machineValStr mv = #2(machineInfo mv)
  fun machineValTy  mv = #3(machineInfo mv)
      
  fun IdxMachineVal n = (case n
      of 0 => SOME MV_Alloc
       | 1 => SOME MV_Vproc
       | _ => NONE
      (* end case *))
      
  val mvCC = [ MV_Alloc, MV_Vproc ] (* parameters added to all basic blocks 
                                       dont change order *)
  
  val numMachineVals = 2
  
  (* end of machine value tools *)
end
