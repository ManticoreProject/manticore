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
    
    (* first type is return type *)
    fun mkLabel name tys = LV.newWithKind(name, LV.VK_Global true, LT.mkFunc tys)
    
  in
    (* NOTE types come from heap-transfer-fn.sml and names from runtime-labels.sml.
       if you add anything here, you should also add it to the list of declared
       functions in the llvm printer. *)
                                
    (*   (vprocPtr * var) -> var   *)
    val promote = mkLabel "PromoteObj" (LT.voidStar :: [LT.voidStar, LT.voidStar])
    
    
    
    (* list of everything in this module for building the declarations *)
    val all = [ promote ]
    
  end
  
  end (* LLVMRuntime *)
