(* llvm-call.sml
 * 
 * COPYRIGHT (c) 2017 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Handles calling conventions and tail calls when translating to LLVM.
 *)

structure LLVMCall : sig

    type convention
    
    type setupInput = (LLVMBuilder.t * LLVMTranslatorUtil.gamma * convention)

    (* given the convention and CFG args, returns an abstract "convention" *)
    val determineCC : { conv : CFG.convention,
                        args : CFG.Var.var list
                      } -> convention
                      
    val determineRet : CFG.Var.var list -> convention
    
    
    
    val setupRetVal : setupInput -> LLVMBuilder.instr
    
    val setupRetCont : setupInput -> LLVMBuilder.instr -> LLVMTranslatorUtil.gamma
    
    (* given a convention, environment, and current status of the LLVM block,
       returns the _full_ list of values to be passed to a function with a matching
       convention. Casts are added as needed. *)
    val setupCallArgs : setupInput -> LLVMBuilder.instr list
    
    val getParamKinds : convention -> LLVMTranslatorUtil.paramKind list
    
    val setupEntryEnv : (LLVMBuilder.t * LLVMTranslatorUtil.gamma * LLVMTranslatorUtil.paramKind list)
                            -> LLVMTranslatorUtil.gamma

    (* turns any filler/fake parameters into new vars, and returns the 
       existing var otherwise *)
    val forceVars : LLVMTranslatorUtil.paramKind list -> LLVMVar.var list

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
    type setupInput = (LLVMBuilder.t * LLVMTranslatorUtil.gamma * convention)
        
    (* fixed padding *)
    val pad : CV.var conv = Filler LT.i64
    val mvs : CV.var conv list = L.map Machine MV.mvCC
    
    (* utils *)
    val getTy = LT.typeOf o CV.typeOf
    val getRegTy = LT.toRegType o getTy
        
        
    (* NOTE this needs to match up with LLVMType.getArgsFor *)
    fun determineCC {conv, args} = (case conv
            of C.StdFunc { clos, ret, exh } => withRegTys (
                    mvs @ L.map Actual ([clos, ret, exh] @ args)
                )

            | (C.StdCont { clos } | C.KnownFunc { clos } | C.KnownDirectFunc {clos, ...}) 
                (* NOTE direct-style uses the KnownDirectFunc conv for cont throws, and
                   those throws come into this func as StdCont, so you should keep those 
                   cases matched up. *)
                => withRegTys (
                        mvs @ [Actual clos, pad, pad] @ (L.map Actual args)
                    )
                
            | C.StdDirectFunc {clos, exh, ret=notAVar} => withRegTys (
                    mvs @ [Actual clos, pad, Actual exh] @ (L.map Actual args)
                )
        (* end case *))
        
    and determineRet args = withRegTys (mvs @ [pad, pad, pad] @ (L.map Actual args))
    
    and withRegTys cs = L.map withRegTy cs
    and withRegTy (Actual cv)  = Actual (getRegTy cv, cv)
      | withRegTy (Machine mv) = Machine mv
      | withRegTy (Filler f)   = Filler f
      
    and projRegTys cs = L.map projRegTy cs
    and projRegTy (Actual (rty, _)) = rty
      | projRegTy (Machine mv) = MV.machineValTy mv
      | projRegTy (Filler rty) = rty
      
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
    
    
    fun setupRetVal (b, env, retConv) = let
    
        val retTy = getRetTy retConv
        
        fun insertElms (Filler _, (i, strct)) = (i+1, strct)
          
          | insertElms (Machine mv, acc) = 
                emitInsert(acc, Util.lookupMV(env, mv))
          
          | insertElms (Actual (_, cv), acc) =
                emitInsert(acc, Util.lookupV(env, cv))
          
        and emitInsert ((i, strct), v) = let 
            val varTy = LB.toTy v
            val slotTy = LT.gevType (retTy, #[i])
            val casted = if LT.same (varTy, slotTy)
                         then v
                         else LB.cast b (Op.safeCast(varTy, slotTy)) (v, slotTy)
        in
            (i+1, LB.insertV b (strct, casted, #[toC i]))
        end
        
        val startStruct = LB.fromC(LB.undef retTy)
        
        val (_, finalStruct) = L.foldl insertElms (0, startStruct) retConv
        
    in
        finalStruct
    end
    
    
    and setupRetCont (b, env, retConv) ret = let
        
        val mvTyOf = MV.machineValTy
        
        fun extractElms (Filler _, (i, env)) = (i+1, env)
          | extractElms (Machine mv, (i, env)) =
                (i+1, Util.updateMV(env, mv, emitExtract (i, MV.machineValTy mv)))
                
          | extractElms (Actual (_, cv), (i, env)) =
                (i+1, Util.insertV(env, cv, emitExtract (i, getTy cv)))
        
        and emitExtract (i, lhsTy) = let
            val extr = LB.extractV b (ret, #[toC i])
            val extrTy = LB.toTy extr
        in
            if LT.same(lhsTy, extrTy)
            then extr
            else LB.cast b (Op.simpleCast(extrTy, lhsTy)) (extr, lhsTy)
        end
        
        val (_, finalEnv) = L.foldl extractElms (0, env) retConv
    in
        finalEnv
    end
    
    and toC i = LB.intC(LT.i32, IntInf.fromInt i)
    and getRetTy retConv = LT.mkUStruct (projRegTys retConv)
    
    
    (*
    (* grab the return values *)
    fun toC i = LB.intC(LT.i32, IntInf.fromInt i)
    
    val (mvAssign, lhsAssign) = determineRet lhs
    
    fun extractElm ret tyOf (i, var) = let
        val extr = LB.extractV b (ret, #[toC i])
        val extrTy = LB.toTy extr
        val lhsTy = tyOf var
    in
        if LT.same(lhsTy, extrTy)
        then extr
        else LB.cast b (Op.simpleCast(extrTy, lhsTy)) (extr, lhsTy)
    end
    
    (* update the machine vals in the env *)
    val newMVs = L.map (extractElm ret MV.machineValTy) mvAssign
    val env = ListPair.foldlEq
                (fn ((_,mv), valu, acc) => Util.updateMV(acc, mv, valu))
                env
                (mvAssign, newMVs)
    
    (* bind the lhs values *)
    val rhs = L.map (extractElm ret (LT.typeOf o CV.typeOf)) lhsAssign
    val env = ListPair.foldlEq
                (fn ((_,lhs), rhs, acc) => Util.insertV(acc, lhs, rhs))
                env
                (lhsAssign, rhs)
    *)
    
    
    fun forceVars ps = L.map forceVar ps
    and forceVar (Util.NotUsed ty) = LV.new("pad", ty)
      | forceVar (Util.Machine (_, lv)) = lv
      | forceVar (Util.Used {llvmParam,...}) = llvmParam
    
    
end (* LLVMCall *)
