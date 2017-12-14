(* llvm-call.sml
 * 
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Handles calling conventions and tail calls when translating to LLVM.
 *)

structure LLVMCall : sig

    type convention

    (* given the convention and CFG args, returns an abstract "convention" *)
    val determineCC : { conv : CFG.convention,
                        args : CFG.Var.var list
                      } -> convention
    
    (* given a convention, environment, and current status of the LLVM block,
       returns the _full_ list of values to be passed to a function with a matching
       convention. Casts are added as needed. *)
    val setupCallArgs : (LLVMBuilder.t * LLVMTranslatorUtil.gamma * convention) -> LLVMBuilder.instr list
    
    val setupEntryBlock : (LLVMBuilder.t * LLVMTranslatorUtil.gamma * convention)
                            -> LLVMTranslatorUtil.gamma

end = struct
    
    structure C = CFG
    structure CV = CFG.Var
    structure CT = CFGTy
    structure L = List
    structure LV = LLVMVar
    structure LT = LV.LT
    structure MV = LLVMMachineVal
    structure Op = LLVMOp
    structure LB = LLVMBuilder
    structure Util = LLVMTranslatorUtil
    
    datatype 'a conv
        = Filler of LT.ty
        | Machine of MV.machineVal
        | Actual of 'a
        
    type convention = (LT.ty * CFG.Var.var) conv list
        
    (* fixed padding *)
    val pad : CV.var conv = Filler LT.i64
    val mvs : CV.var conv list = L.map Machine MV.mvCC
    
    (* utils *)
    val getTy = LT.toRegType o LT.typeOf o CV.typeOf
        
        
    
    fun determineCC {conv, args} = (case conv
            of C.StdFunc { clos, ret, exh } => withTys (
                    mvs @ L.map Actual ([clos, ret, exh] @ args)
                )

            | (C.StdCont { clos } | C.KnownFunc { clos } | C.KnownDirectFunc {clos, ...}) 
                (* NOTE direct-style uses the KnownDirectFunc conv for cont throws, and
                   those throws come into this func as StdCont, so you should keep those 
                   cases matched up. *)
                => withTys (
                        mvs @ [Actual clos, pad, pad] @ (L.map Actual args)
                    )
                
            | C.StdDirectFunc {clos, exh, ret=notAVar} => withTys (
                    mvs @ [Actual clos, pad, Actual exh] @ (L.map Actual args)
                )
        (* end case *))
    
    and withTys cs = L.map withTy cs
    and withTy (Actual cv)  = Actual (getTy cv, cv)
      | withTy (Machine mv) = Machine mv
      | withTy (Filler f)   = Filler f
      
      
    fun setupCallArgs (b, env, conv) = let
    
        fun convert (Filler ty) = LB.fromC(LB.undef ty)
          | convert (Machine mv) = Util.lookupMV(env, mv)
          | convert (Actual (regTy, cfgV)) = let
                val llvmV = Util.lookupV(env, cfgV)
                val llvmTy = LB.toTy llvmV
              in
                LB.cast b (Op.safeCast (llvmTy, regTy)) (llvmV, regTy)
              end
    in
        L.map convert conv
    end
    
    fun setupEntryBlock _ = raise Fail "todo"
    
end (* LLVMCall *)
