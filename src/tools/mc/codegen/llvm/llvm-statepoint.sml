(* llvm-statepoint.sml
 * 
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * GC support for stack-based continuations in LLVM
 *)

structure LLVMStatepoint : sig

    val call :  
              { blk : LLVMBuilder.t,
                conv : LLVMBuilder.convention,
                func : LLVMBuilder.instr,
                args : LLVMBuilder.instr list,
                lives : LLVMBuilder.instr list 
              } -> {
                ret : LLVMBuilder.instr,
                relos : LLVMBuilder.instr list
              }
                  
                

end = struct

    (* TODO:
        - keep a cache of statepoint intrinsic functions
          that you have emitted calls for so you can
          export that later.
        - actually emit the calls.
    *)

    fun call _ = raise Fail "implement me"

end (* LLVMStatepoint *)
