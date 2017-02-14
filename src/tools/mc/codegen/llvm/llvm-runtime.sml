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
    fun mkLabel name tys = mkConst name (LT.mkPtr(LT.mkFunc tys))
    
    and mkConst name ty = LV.newWithKind(name, LV.VK_Global true, ty)
    
  in
  
    (**************************************
        MANTICORE RUNTIME LABELS
    ***************************************)
    
    (* these need external visibility *)
    val magic = mkConst "mantMagic" LT.i32
    val sequential = mkConst "SequentialFlag" LT.i32
    fun main ty = mkConst "mantEntry" ty
    
      
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
            
            val stdRegSetDS = [LT.allocPtrTy, LT.vprocTy]
            val retStructTyDS = LT.mkUStruct(stdRegSetDS)
            
    (* we need a special InvokeGC for LLVM *)
    val invokeGC : LV.var * (LB.convention option) = (mkLabel "ASM_InvokeGC_LLVM" (retStructTy :: stdRegSet), SOME LB.jwaCC)
    val dsInvokeGC : LV.var * (LB.convention option) = (mkLabel "ASM_InvokeGC_DS_LLVM" (retStructTyDS :: stdRegSetDS), SOME LB.jwaCC)
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
    
    (* used to tell the optimizer which branches are expected to be taken to guide block placement.
       args are (i1 <val>, i1 <expected_val>) *)
    val expect_i1 : LV.var * (LB.convention option) = (mkLabel "llvm.expect.i1" (LT.i1 :: [ LT.i1, LT.i1 ]), NONE)
    
    
    (* list of everything in this module for building the declarations. the LLVM printer
       will automatically output anything in these lists for you. *)
    val runtime = [ promote,
                    invokeGC,
                    dsInvokeGC,
                    
                    (* Some of the below array/vector allocation externs are commented
                       out because they're already included in the BOM module, and this
                       would be a redefinition. Rest assured that the functions in this
                       list, commented out or not, are the ones being used by these
                       primops, however. I just did not want to sift through
                       the BOM module and MLRISC backend to make it consistent. *)
                       
                    (* allocVector, *)
                    allocIntArray,
                    allocLongArray,
                    allocFloatArray,
                    allocDoubleArray
                  ]
    
    val intrinsics = [  
                        sqrt_f32,
                        sqrt_f64,
                        abs_f32,
                        abs_f64,
                        readtsc,
                        expect_i1 ]
    
  end
  
  end (* LLVMRuntime *)
