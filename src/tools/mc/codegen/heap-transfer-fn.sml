(* heap-transfer-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Generate CFG calls and entry points using heap-allocated activation records.
 *)

signature TARGET_TRANSFER_HEAP = sig
    type stm
	 
    val genGCCall : unit -> stm list
end (* TARGET_TRANSFER_HEAP *)

functor HeapTransferFn (
    structure MTy : MLRISC_TYPES
    structure VarDef : VAR_DEF where MTy = MTy
    structure SpillLoc : SPILL_LOC 
    structure Copy : COPY where MTy = VarDef.MTy
    structure Regs : MANTICORE_REGS
    structure Alloc : ALLOC
	where MTy = MTy
    structure MLTreeComp : MLTREECOMP where TS.T = VarDef.MTy.T
    structure Target : TARGET_TRANSFER_HEAP
	where type stm = VarDef.MTy.T.stm
    structure Spec : TARGET_SPEC
    structure LabelCode : LABEL_CODE
	where MTy = MTy
    structure Types : ARCH_TYPES
    structure CCall : C_CALL
	where T = MTy.T
    structure Frame : MANTICORE_FRAME
    structure VProcOps : VPROC_OPS
	where VarDef = VarDef
	where MTy = MTy
) : TRANSFER = struct

  structure MTy = MTy
  structure T = MTy.T
  structure VarDef = VarDef
  structure SpillLoc = SpillLoc
  structure Cells = MLTreeComp.I.C
  structure M = CFG
  structure Var = M.Var
  structure Frame = Frame

  type stms = MTy.T.stm list

  val apReg = Regs.apReg
  val ty = MTy.wordTy
  val iTy = Types.szOf (CFGTy.T_Raw CFGTy.T_Int)
  val memory = ManticoreRegion.memory

  val kfncRegs = Regs.argRegs
  val stdFuncRegs as [closReg, argReg, retReg, exhReg] = 
      [Regs.closReg, Regs.argReg, Regs.retReg, Regs.exhReg]
  val stdContRegs as [closReg, argReg] = 
      [Regs.closReg, Regs.argReg]

  val szOfVar = Types.szOf o Var.typeOf
  fun newLabel s = Label.label s () 
  fun regExp r = T.REG (MTy.wordTy, r)
  fun move (r, e) = T.MV (MTy.wordTy, r, e)
  fun move' (r, mlt) = 
      (case MTy.treeToMLRisc mlt
	of T.GPR e => move (r, e)
	 | _ => raise Fail "move'"
      (* end case *))
  fun newReg _ = Cells.newReg ()
  fun newFReg _ = Cells.newFreg ()
  fun gpReg r = MTy.GPReg (MTy.wordTy, r)
  fun mltGPR r = MTy.GPR (MTy.wordTy, r)
  fun regGP (MTy.GPReg (_, r)) = r
  fun mkExp rexp = MTy.EXP (MTy.wordTy, rexp)
  val toGPR = T.GPR o regExp o regGP
  fun mlrReg v = 
      let val mty = Var.typeOf v
      in
	  (case MTy.cfgTyToMLRisc mty
	    of MTy.K_INT => MTy.GPReg (Types.szOf mty, newReg ())
	     | MTy.K_FLOAT => MTy.FPReg (Types.szOf mty, newFReg ())
	  (* end case *))
      end (* mlrReg *)

  (* emit a jump by first copying argument registers to parameter registers and then emitting a jump instruction. *)
  fun genJump (target, ls, params, args) =
      let val stms = Copy.copy {src=args, dst=params}
      in
	  stms @ [T.JMP (target, ls)]
      end (* genJump *)

  fun genGoto varDefTbl (l, args) =
      let val useDefOf = VarDef.useDefOf varDefTbl
	  val name = LabelCode.getName l
	  val params = LabelCode.getParamRegs l
	  val args' = map useDefOf args
	  val argRegs = map (fn (MTy.GPReg (ty, _)) => MTy.GPReg (ty, newReg())
			      | (MTy.FPReg (ty, _)) => MTy.FPReg (ty, newFReg()) )
			params
	  val stms = Copy.copy {src=args', dst=argRegs}
      in
	  stms @ genJump (T.LABEL name, [name], params, map MTy.regToTree argRegs)
      end (* genGoto *)

  fun genStdTransfer varDefTbl (tgtReg, args, argRegs, stdRegs) =
      let val getDefOf = VarDef.getDefOf varDefTbl
	  val stdRegs = map gpReg stdRegs 
      in
	  {stms=List.concat [
	   (* copy the arguments into temp registers *)
	   Copy.copy {src=map getDefOf args, 
		      dst=map gpReg argRegs},
	   (* jump to the function with fresh arguments *)
	   genJump (tgtReg, [] (* FIXME: get these values from CFA *), stdRegs, map mltGPR argRegs)],
	   liveOut=map toGPR stdRegs}
      end (* genStdTransfer *)

  (* if labExp is a label, put it into the jump directly, ow use a register temp. *)
  fun genTransferTarget labExp = (case labExp
       of T.LABEL _ => (labExp, [])
	| _ => let val tgtReg = newReg ()
	       in (regExp tgtReg, [move (tgtReg, labExp)]) end
       (* end case *))

  fun genApply varDefTbl {f, args} = 
      let val defOf = VarDef.defOf varDefTbl
	  val argRegs = map newReg args
          val kfncRegs = List.take (kfncRegs, length argRegs)
	  val (lab, mvInstr) = genTransferTarget (defOf f)
	  val {stms, liveOut} =
	      genStdTransfer varDefTbl (lab, args, argRegs, kfncRegs)
      in
	  {stms=mvInstr @ stms, liveOut=liveOut}
      end 
    (* genApply *)

  fun genStdApply varDefTbl {f, clos, args as [arg], ret, exh} = 
      let val defOf = VarDef.defOf varDefTbl 
          val args = [clos, arg, ret, exh] 
          val argRegs = map newReg args 
	  val (lab, mvInstr) = genTransferTarget (defOf f)
          val {stms, liveOut} = 
	      genStdTransfer varDefTbl (lab, args, argRegs, stdFuncRegs) 
      in
	  {stms=mvInstr @ stms, liveOut=liveOut}
      end 
    | genStdApply _ _ = raise Fail "genStdApply: ill-formed StdApply"
    (* genStdApply *)

  fun genStdThrow varDefTbl {k, clos, args as [arg]} = 
      let val defOf = VarDef.defOf varDefTbl
          val args = [clos, arg]
	  val argRegs = map newReg args
	  val (labK, mvInstr) = genTransferTarget (defOf k)
	  val {stms, liveOut} = 
	      genStdTransfer varDefTbl (labK, args, argRegs, stdContRegs)
      in 
	  {stms=mvInstr @ stms, liveOut=liveOut}
      end 
    | genStdThrow _ _ = raise Fail "genStdThrow: ill-formed StdThrow"
    (* genStdThrow *)

  structure Ty = CFGTy
  structure CTy = CTypes

  fun rawTyToCTy Ty.T_Byte = CTy.C_signed CTy.I_char
    | rawTyToCTy Ty.T_Short = CTy.C_signed CTy.I_short
    | rawTyToCTy Ty.T_Int = CTy.C_signed CTy.I_int
    | rawTyToCTy Ty.T_Long = CTy.C_signed CTy.I_long_long
    | rawTyToCTy Ty.T_Float = CTy.C_float
    | rawTyToCTy Ty.T_Double = CTy.C_double
    | rawTyToCTy Ty.T_Vec128 = raise Fail "todo"

  fun cfgTyToCTy ty = (case ty
	 of Ty.T_Any => CTy.C_PTR
	  | Ty.T_Raw rt => rawTyToCTy rt
	  | Ty.T_Enum _ => CTy.C_signed CTy.I_int
	  | Ty.T_Tuple _ => CTy.C_PTR
	  | Ty.T_OpenTuple _ => CTy.C_PTR
	  | Ty.T_VProc => CTy.C_PTR
	  | _ => raise Fail(concat["cfgTyToCTy(", CFGTy.toString ty, ")"])
	(* end case *))

  fun cvtCTy (CFunctions.PointerTy) = CTy.C_PTR
    | cvtCTy (CFunctions.BaseTy rTy) = rawTyToCTy rTy
    | cvtCTy (CFunctions.VoidTy) = CTy.C_void
				   
  (* remove a register from a list of registers *)
  fun removeReg reg (r::rs) = if CellsBasis.sameCell(reg, r)
      then rs
      else removeReg reg rs
    | removeReg _ [] = []

 (* assign the host vproc pointer to a register *)
  fun hostVProc () = let
        val r = newReg()
	val MTy.EXP(_, hostVP) = VProcOps.genHostVP
        in
          (T.REG(ty, r), T.MV(ty, r, hostVP))
        end

  fun ccall {lhs, name, retTy, paramTys, cArgs, saveAllocPtr} = let
      val {callseq, result} = CCall.genCall {
	    name=name, 
	    args=cArgs,
	    proto={conv="", retTy=retTy, paramTys=paramTys},
	    paramAlloc=fn _ => false,
	    structRet=fn _ => T.REG (64, retReg),
	    saveRestoreDedicated=fn _ => {save=[], restore=[]},
	    callComment=NONE
	  } 
      (* for each caller-save register, allocate a fresh temporary and
       * generate save/restore operations that copy the dedicated registers
       * to/from the temporaries.
       *)
      fun saveRegs rs = let
	  fun loop ([], (tmps, ss)) = {
	        saves=T.COPY (ty, tmps, ss),
		restores=T.COPY (ty, ss, tmps)
	      }
	    | loop (r :: rs, (tmps, ss)) =
	      loop (rs, (newReg () :: tmps, r :: ss))
          in
	    loop (rs, ([], []))
          end (* saveRegs *)
      val {saves, restores} = if saveAllocPtr
	  then saveRegs (removeReg Regs.apReg Regs.saveRegs)
	  else saveRegs Regs.saveRegs
      (* we need a pointer to the vproc to set the inManticore flag, etc. *)
      val (vpReg, setVP) = hostVProc ()
      (* generate a statement to store a value in the vproc inManticore flag *)
      fun setInManticore value = 
	  VProcOps.genVPStore' (Spec.ABI.inManticore, vpReg, T.LI value)
      (* statements to save/restore the allocation pointer from the vproc *)
      val (saveAP, restoreAP) = if saveAllocPtr
    	  then let
             val apReg = T.REG(ty, Regs.apReg)
	     val save = VProcOps.genVPStore' (Spec.ABI.allocPtr, vpReg, apReg)
	     val restore = T.MV(ty, Regs.apReg,
				VProcOps.genVPLoad' (Spec.ABI.allocPtr, vpReg))
             in
	        ([save], [restore])
             end
           else ([], [])
      val stms = setVP :: saveAP @ [setInManticore(Spec.falseRep), saves]
		 @ callseq
		 @ (restores :: restoreAP) @ [setInManticore(Spec.trueRep)]
      fun convResult (T.GPR e, v) = MTy.EXP (szOfVar v, e)
	| convResult (T.FPR e, v) = MTy.FEXP (szOfVar v, e)
	| convResult _ = raise Fail "convResult"
      val result = ListPair.map convResult (result, lhs)
      in
          {stms=stms, result=result}
      end

  fun mlriscToCArg (T.GPR rexp) = CCall.ARG rexp
    | mlriscToCArg (T.FPR fexp) = CCall.FARG fexp
    | mlriscToCArg _ = raise Fail "impossible"
 (* convert a CFG variable to a C argument *)
  fun varToCArg varDefTbl = mlriscToCArg o MTy.treeToMLRisc o (VarDef.getDefOf varDefTbl)

  fun getCPrototype f = (case Var.typeOf f
      of  CFGTy.T_CFun proto => proto
	| _ => raise Fail(concat["HeapTransferFn.getCPrototype: ", Var.toString f, " not a C function"])
      (* end case *))
  
  fun genCCall varDefTbl {lhs, f, args} = let
      (* get the C function's prototype *)
      val cProtoTy as CFunctions.CProto(retTy, paramTys, _) = getCPrototype f
      (* do we need to save/restore the allocation pointer? *)
      val saveAllocPtr = CFunctions.protoHasAttr CFunctions.A_alloc cProtoTy
      in
        ccall {lhs=lhs, 
	       name=VarDef.defOf varDefTbl f, 
	       retTy=cvtCTy retTy, paramTys=List.map cvtCTy paramTys, 
	       cArgs=map (varToCArg varDefTbl) args, saveAllocPtr=saveAllocPtr}
      end (* genCCall *)
      
  (* Promote an object to the global heap.
   *     
   * NOTE: this call might trigger a global GC.
   *)
  fun genPromote varDefTbl {lhs, arg} = 
      ccall {lhs=[lhs], 
	     name=T.LABEL RuntimeLabels.promote,
	     retTy=CTy.C_PTR, paramTys=[CTy.C_PTR, CTy.C_PTR], 
	     cArgs=[CCall.ARG VProcOps.genHostVP', varToCArg varDefTbl arg], saveAllocPtr=true}

  (* take the CFG variables for the GC roots and return MLRISC code that initializes and restores
   * the roots and also return the root pointer, register temps for the roots, and values for the roots
   *)
  fun processGCRoots varDefTbl (roots : CFG.var list, restoreLoc) = let
     (* preprocess the roots *)
      fun loop ([], tys, args, temps) = (rev tys, rev args, rev temps)
	| loop (root::roots, tys, args, temps) = let
	   val ty = Var.typeOf root
	   val arg = VarDef.getDefOf varDefTbl root
	   val temp = mlrReg root 
	   in
	      loop (roots, ty::tys, arg::args, temp::temps)
	   end
     (* types, values, and temporary registers for the roots (order matters) *)
      val (rootTys, rootArgs, rootTemps) = loop (roots, [], [], [])
     (* allocate the roots *)
      val {ptr=rootPtr, stms=initRoots} = Alloc.genAlloc (ListPair.zip (rootTys, map MTy.regToTree rootTemps))
     (* restore the roots *)
      fun restore ([], i, rs) = rev rs
	| restore (ty::tys, i, rs) = let
            val r = Alloc.select {lhsTy=Types.szOf ty, mty=M.T_Tuple (false, rootTys), i=i, base=restoreLoc}
	    in
	      restore (tys, i+1, r::rs)
	    end
      val restoredRoots = restore (rootTys, 0, [])
      in
         { initRoots=initRoots, restoredRoots=restoredRoots, rootPtr=rootPtr, rootTemps=rootTemps, rootArgs=rootArgs }
      end (* processGCRoots *)

  fun genAllocCCall' varDefTbl (fLabel, CFunctions.CProto(retTy, paramTys, _), fArgs, retFun, roots) = let
      val retLabel = LabelCode.getName retFun
      val (vpReg, setVP) = hostVProc ()
      val stdEnvPtrOffset = VProcOps.genVPAddrOf (Spec.ABI.stdEnvPtr, vpReg)
      val {initRoots, restoredRoots, rootPtr, rootTemps, rootArgs} = processGCRoots varDefTbl (roots, stdEnvPtrOffset)
     (* if the C call returns a value (it is non-void), we pass the return value to the first argument 
      * position of the return function (ret) 
      *)
      val lhs = (case retTy
	  of CFunctions.VoidTy => []
	   | _ => [hd roots]
          (* end case *))
      val {stms=stmsC, result=resultC} = ccall {lhs=lhs, name=fLabel, 
               retTy=cvtCTy retTy, paramTys=List.map cvtCTy paramTys, 
	       cArgs=fArgs, saveAllocPtr=false}
      in
         List.concat [
           (* store dedicated registers and the rootset pointer before entering the C call *)
	    [ (* by convention we put the rootset pointer into stdEnvPtr *)
              VProcOps.genVPStore' (Spec.ABI.stdEnvPtr, vpReg, MTy.mlriscTreeToRexp rootPtr),
	      VProcOps.genVPStore' (Spec.ABI.allocPtr, vpReg, regExp Regs.apReg),
	      VProcOps.genVPStore' (Spec.ABI.limitPtr, vpReg, regExp Regs.limReg) 
	    ],
            stmsC,
	   (* restore dedicated registers and the roots *) 
	    [
	     move (Regs.apReg, (VProcOps.genVPLoad' (Spec.ABI.allocPtr, vpReg))),
	     move (Regs.limReg, (VProcOps.genVPLoad' (Spec.ABI.limitPtr, vpReg)))
	    ],
	   (* jump to the return post-C-call function *)
	    genJump (T.LABEL retLabel, [retLabel], rootTemps, restoredRoots)	    
	 ]
      end (* genAllocCCall' *)

 (* apply a C funcion f to args (f can trigger a garbage collection). *)
  fun genAllocCCall varDefTbl {f, args, ret=ret as (retFun, roots)} = let
      val fLabel = VarDef.defOf varDefTbl f
      val fPrototype = getCPrototype f
      val fArgs = map (varToCArg varDefTbl) args
      in
        genAllocCCall' varDefTbl (fLabel, fPrototype, fArgs, retFun, roots)
      end

  (* Generate either a global or local heap check.
   *
   * We check if it is necessary to transfer control into the runtime system.  This can happen for
   * two reasons: there is insufficent space in the local or global heaps (obvious) or the vproc received
   * a preemption.  In the latter case, the runtime might execute other allocating code, and thus the 
   * heap could have insufficient space after returning from the runtime system.
   *
   * The heap check works as follows. If the check succeeds, apply the nogc function.  Otherwise, 
   * transfer into the runtime system by doing these steps:
   *   1. Allocate the root set in the heap (we dedicate heap slop space to ensure this will work).
   *   2. Put the root set pointer into the closure register, and put the return continuation into the 
   *      return retister
   *   3. Call the GC initialization routine passing it the return continuation and the root set.
   *
   * The return continuation is somewhat unusual, as it jumps right back to the heap limit test.  It 
   * must do this step for the reason mentioned earlier: the heap might still have insufficient space
   * because of preemption.
   *)
  fun genHeapCheck varDefTbl {hck, szb, nogc=(noGCLbl, roots)} = let
      val {initRoots, restoredRoots, rootPtr, rootTemps, rootArgs } = processGCRoots varDefTbl (roots, regExp closReg)
								      
      val noGCParamRegs = LabelCode.getParamRegs noGCLbl
      val noGCLbl = LabelCode.getName noGCLbl
      val retLbl = newLabel "retGC"
      val gcTestLbl = newLabel "gcTest"
		      
     (* perform the GC *)
      val doGCLbl = newLabel "doGC"
      val doGCStms = List.concat [
 	      [T.DEFINE doGCLbl],	      
	     (* allocate a heap object for GC roots *)
	      initRoots,
	     (* save the root pointer in the closure register *)
	      [move' (closReg, rootPtr)],
	     (* put the return address into retReg *)
	      [move (retReg, T.LABEL retLbl)],
	     (* jump to the garbage collector *)
	      Target.genGCCall () 
          ]

     (* jump to the heap limit check *)
      val retStms = genJump (T.LABEL gcTestLbl, [gcTestLbl], rootTemps, restoredRoots)
	      
     (* generate code for the type of allocation check *)
      val {stms=allocCheckStms, allocCheck} = (case hck
  	  of CFG.HCK_Local => {stms=[], allocCheck=Alloc.genAllocCheck szb}
	   | CFG.HCK_Global => Alloc.genGlobalAllocCheck szb
          (* end case *))

      val stms = List.concat [
                 (* force the root set into registers *)
		  Copy.copy {dst=rootTemps, src=rootArgs},
		  [T.DEFINE gcTestLbl],	      
		 (* branch on the heap limit test *)
		  allocCheckStms,
		  [T.BCC (allocCheck, doGCLbl)],
		 (* GC is unnecessary *)
		  genJump (T.LABEL noGCLbl, [noGCLbl], noGCParamRegs, map MTy.regToTree rootTemps),
		 (* GC is necessary *)
		  doGCStms 
             ] 
      in	  
	  {stms=stms, retLbl=retLbl, retStms=retStms, liveOut=map MTy.gprToExp noGCParamRegs}
      end (* genHeapCheck *)

 (* entry code for a function *)
  fun genFuncEntry varDefTbl (lab, convention) = let
        datatype conv = 
               (* specialized calling convention or block *)
                Special 
 	       (* standard calling convention *) 
	      | StdConv of Regs.gpr list
       (* set the parameters according to the calling convention *)
	val (params, stdRegs) = (case convention
	    of M.StdFunc{clos, args as [arg], ret, exh} => ([clos, arg, ret, exh], StdConv stdFuncRegs)
             | M.StdFunc _ => raise Fail "genFuncEntry: ill-formed StdFunc"
	     | M.StdCont{clos, args as [arg]} => ([clos, arg], StdConv stdContRegs)
             | M.StdCont _ => raise Fail "genFuncEntry: ill-formed StdCont"
             | M.KnownFunc args => (args, StdConv (List.take (kfncRegs, length args)))
	     | M.Block vs => (vs, Special)
	    (* end case *))
       (* copy params into param registers *)
	fun gpReg' (param, paramReg) = MTy.GPReg (szOfVar param, paramReg)
	val {stms, regs=paramRegs} = (case stdRegs
	    of Special => {stms=[], regs=map mlrReg params}
	     | StdConv stdRegs => Copy.fresh (ListPair.map gpReg' (params, stdRegs))
            (* end case *))
        in 	
         (* make param registers available globally *)
	  LabelCode.setParamRegs (lab, paramRegs);
         (* bind params to their registers *)
	  ListPair.app (VarDef.setDefOf varDefTbl) (params, map MTy.regToTree paramRegs);
	  stms
         end (* genFuncEntry *)

end (* HeapTransferFn *)
