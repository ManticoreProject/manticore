(* llvm-runtime.sml
 * 
 * COPYRIGHT (c) 2016 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Fixed labels to interface with the outside world via the LLVM backend.
 *)

structure LLVMRuntime =
  struct
  
  
  local
    structure LV = LLVMVar
    structure LT = LLVMType
    structure LB = LLVMBuilder
    
    (* first type is return type *)
    fun mkLabel name tys = LV.newWithKind(name, LV.VK_Global true, LT.mkFunc tys)
    
  in
  
    (**************************************
        MANTICORE RUNTIME LABELS
    ***************************************)
    
    (*
    (* entry point for main program *)
      val entry = global "mantEntry"
    (* label of word that holds "magic number".  This value used to check the
     * consistency between the runtime and compiler offsets.
     *)
      val magic = global "mantMagic"
    (* label of flag that tells the runtime if the generated code is sequential *)
      val sequential = global "SequentialFlag"
    (* runtime code to invoke the GC *)
      val initGC = global "ASM_InvokeGC"
    (* runtime code to promote objects *)
      val promote = global "PromoteObj"
    
    (* runtime code to get a new global-heap chunk *)
      val getGlobalChunk = global "GetChunkForVProc"
      
      val allocVector = global "AllocVector"
      val allocIntArray = global "AllocIntArray"
      val allocLongArray = global "AllocLongArray"
      val allocFloatArray = global "AllocFloatArray"
      val allocDoubleArray = global "AllocDoubleArray"
    *)
  
    (* NOTE types come from heap-transfer-fn.sml and names from runtime-labels.sml.
       if you add anything here, you should also add it to the list of declared
       functions in the llvm printer. the 2nd item in the tuple is the calling convention.
       none means to not specify a calling convention. *)
                                
    (*   (vprocPtr * var) -> var   *)
    val promote : LV.var * (LB.convention option) = (mkLabel "PromoteObj" (LT.voidStar :: [LT.voidStar, LT.voidStar]), NONE)
    
    (* aka AllocPolyVec. (vprocPtr * any) -> any *)
    val allocVector : LV.var * (LB.convention option) = (mkLabel "AllocVector" (LT.voidStar :: [LT.voidStar, LT.voidStar]), NONE)
    
    (*  the following alloc functions have signature (vprocPtr * int) -> any *)
    val allocIntArray : LV.var * (LB.convention option) = (mkLabel "AllocIntArray" (LT.voidStar :: [LT.voidStar, LT.i32]), NONE)
    val allocLongArray : LV.var * (LB.convention option) = (mkLabel "AllocLongArray" (LT.voidStar :: [LT.voidStar, LT.i32]), NONE)
    val allocFloatArray : LV.var * (LB.convention option) = (mkLabel "AllocFloatArray" (LT.voidStar :: [LT.voidStar, LT.i32]), NONE)
    val allocDoubleArray : LV.var * (LB.convention option) = (mkLabel "AllocDoubleArray" (LT.voidStar :: [LT.voidStar, LT.i32]), NONE)
    
        local
        in
            val stdRegSet = [LT.allocPtrTy, LT.vprocTy, LT.uniformTy]
            val retStructTy = LT.mkUStruct(stdRegSet)
            
    val invokeGC : LV.var * (LB.convention option) = (mkLabel "ASM_InvokeGC" (retStructTy :: stdRegSet), SOME LB.jwaCC)
        end
        
    
    
    (***************************************
        LLVM INTRINSICS
    ***************************************)
    
    (* Many of these are overloaded intrinsics. 
    You can use llvm.sqrt on any floating point or vector of floating point type, for example. 
    Not all targets support all types however, so cross your fingers and hope it works! *)
    
    (* TODO currently a lot of other math functions that are available in LLVM ought to
           be what we use in place of stuff like M_Cos M_Sin in the runtime system.
           we should be able to achieve this pretty painlessly if we overwrite in the environment the initial mappings from CFG labels to LLVMVars that use
           the externed C functions with our instrinsic functions instead, because
           they have the exact same type signatures anyways. *)
    
    val sqrt_f32 : LV.var * (LB.convention option) = (mkLabel "llvm.sqrt.f32" (LT.floatTy :: [ LT.floatTy ]), NONE)
    val sqrt_f64 : LV.var * (LB.convention option) = (mkLabel "llvm.sqrt.f64" (LT.doubleTy :: [ LT.doubleTy ]), NONE)
    
    val abs_f32 : LV.var * (LB.convention option) = (mkLabel "llvm.fabs.f32" (LT.floatTy :: [ LT.floatTy ]), NONE)
    val abs_f64 : LV.var * (LB.convention option) = (mkLabel "llvm.fabs.f64" (LT.doubleTy :: [ LT.doubleTy ]), NONE)
    
    val readtsc : LV.var * (LB.convention option) = (mkLabel "llvm.readcyclecounter" (LT.i64 :: nil), NONE)
    
    
    (* list of everything in this module for building the declarations. the LLVM printer
       will automatically output anything in these lists for you. *)
    val runtime = [ promote,
                    invokeGC
                (*    , 
                
                 TODO holy crap this alloc vector stuff is so messed up!
                        what is the difference between AllocBigIntArray that is declared
                        in the CFG module and AllocIntArray which is declared in
                        runtime-labels.sml and in the runtime system C code?!?!?!
                        there's even an MLRISC based version of polyvec, so there's
                        seriously 3 implementations hanging around!
                        
                    allocVector,
                    allocIntArray,
                    allocLongArray,
                    allocFloatArray,
                    allocDoubleArray *)
                    ]
    
    val intrinsics = [  
                        sqrt_f32,
                        sqrt_f64,
                        abs_f32,
                        abs_f64,
                        readtsc ]
    
  end
  
  end (* LLVMRuntime *)
