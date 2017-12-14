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
    
    val getParamKinds : convention -> LLVMTranslatorUtil.paramKind list
    
    val setupEntryEnv : (LLVMBuilder.t * LLVMTranslatorUtil.gamma * LLVMTranslatorUtil.paramKind list)
                            -> LLVMTranslatorUtil.gamma

    (* turns any filler/fake parameters into new vars, and returns the 
       existing var otherwise *)
    val getVars : LLVMTranslatorUtil.paramKind list -> LLVMVar.var list

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
    val getTy = LT.typeOf o CV.typeOf
    val getRegTy = LT.toRegType o getTy
        
        
    
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
    and withTy (Actual cv)  = Actual (getRegTy cv, cv)
      | withTy (Machine mv) = Machine mv
      | withTy (Filler f)   = Filler f
      
      
    and getParamKinds cs = L.map getParamKind cs
    and getParamKind (Actual (regTy, cv)) = let
            val llv = LV.new(CV.nameOf cv, regTy)
            val actualTy = getTy cv
        in
            Util.Used {cfgParam = cv,
                       llvmParam = llv,
                       realTy = actualTy}
        end
        
      | getParamKind (Machine mv) =
            Util.Machine (mv, LV.new(MV.machineValStr mv, MV.machineValTy mv))
            
      | getParamKind (Filler ty) = Util.NotUsed ty
      
      
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
    
    fun setupEntryEnv (b, env, startConv) = let
        
        fun insert (p, env) = (case p
            of Util.NotUsed _ => env
             | Util.Machine (mv, llv) => Util.updateMV(env, mv, LB.fromV llv)
             | Util.Used {cfgParam, llvmParam, realTy} => let
                    val castPair = (LV.typeOf llvmParam, realTy)
                    val argPair = (LB.fromV llvmParam, realTy)
                    val newVar = LB.cast b (Op.simpleCast castPair) argPair
                 in
                    Util.insertV(env, cfgParam, newVar)
                 end
            (* esac *))
    in
        L.foldl insert env startConv
    end
    
    fun getVars ps = L.map getVar ps
    and getVar (Util.NotUsed ty) = LV.new("pad", ty)
      | getVar (Util.Machine (_, lv)) = lv
      | getVar (Util.Used {llvmParam,...}) = llvmParam
    
    
    
end (* LLVMCall *)
