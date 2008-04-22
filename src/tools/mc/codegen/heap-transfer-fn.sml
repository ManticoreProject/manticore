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
end

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
  structure Ty = CFGTy
  structure CTy = CTypes

  type stms = MTy.T.stm list

  val stdFuncRegs = [Regs.closReg, Regs.argReg, Regs.retReg, Regs.exhReg]
  val stdContRegs = [Regs.closReg, Regs.argReg]

  fun newLabel s = Label.label s () 
  fun newReg _ = Cells.newReg ()
  fun newFReg _ = Cells.newFreg ()

  fun regTree r = T.REG (MTy.wordTy, r)

  fun move (r, e) = T.MV (MTy.wordTy, r, e)

  (* create a fresh MLRISC register for a CFG variable *)
  fun varToFreshReg v = let
      val ty = Var.typeOf v
      in 
         case MTy.cfgTyToMLRisc ty
	  of MTy.K_INT => MTy.GPReg (Types.szOf ty, newReg ())
	   | MTy.K_FLOAT => MTy.FPReg (Types.szOf ty, newFReg ())
      end

 (* assign the host vproc pointer to a register *)
  fun hostVProc () = let
        val r = newReg()
	val MTy.EXP(_, hostVP) = VProcOps.genHostVP
        in
          (T.REG(MTy.wordTy, r), T.MV(MTy.wordTy, r, hostVP))
        end

  datatype location_kind = K_GPR | K_FPR | K_MEM

  structure SA = StagedAllocationFn (
                   structure T = T
		   structure TargetLang =
                      struct
		          datatype location_kind = datatype location_kind
		      end
		   val memSize = 8)

  structure CallingConventions =
    struct

	fun stdApply () = let
	    val cGpr = SA.freshCounter()
	    val states = [
		SA.WIDEN (fn w => Int.max(MTy.wordTy, w)),
		SA.ARGCOUNTER cGpr,
		SA.REGS_BY_ARGS (cGpr, List.map (fn r => (MTy.wordTy, r)) stdFuncRegs)
	    ]
	    in
	       {c0=SA.init [cGpr], cStep=SA.mkStep states, finish=fn _ => raise Fail "impossible"}
            end

	fun stdCont () = let
	    val cGpr = SA.freshCounter()
	    val states = [
		SA.WIDEN (fn w => Int.max(MTy.wordTy, w)),
		SA.ARGCOUNTER cGpr,
		SA.REGS_BY_ARGS (cGpr, List.map (fn r => (MTy.wordTy, r)) stdContRegs)
	    ]
	    in
	       {c0=SA.init [cGpr], cStep=SA.mkStep states, finish=fn _ => raise Fail "impossible"}
            end

	fun apply () = let
	    val cScratch = SA.freshCounter()
	    val cGpr = SA.freshCounter()
	    val cFpr = SA.freshCounter()
	    val states = [
		SA.CHOICE [
		(* pass in general-purpose register *)
		(fn (w, k, str) => k = K_GPR, 
		 SA.SEQ [
		 SA.WIDEN (fn w => Int.max(MTy.wordTy, w)),
		 SA.ARGCOUNTER cGpr,
		 SA.REGS_BY_ARGS (cGpr, List.map (fn r => (MTy.wordTy, r)) Regs.argRegs) 
		]),
		(* pass in floating-point register *)
		(fn (w, k, str) => k = K_FPR, 
		 SA.SEQ [
		 SA.WIDEN (fn w => List.foldl Int.max w Regs.fprWidths),
		 SA.ARGCOUNTER cFpr,
		 SA.REGS_BY_ARGS (cFpr, List.map (fn r => (64, r)) Regs.argFRegs) 
                ])],
		(* pass in scratch space *)
		SA.OVERFLOW {counter=cScratch, blockDirection=SA.UP, maxAlign=List.foldl Int.max 0 Regs.gprWidths div 8}
	    ]
	    in
	       {c0=SA.init [cScratch, cGpr, cFpr], cStep=SA.mkStep states, finish=fn str => SA.find(str, cScratch)}
            end
    end (* CallingConventions *)

  (* if labExp is a label, put it into the jump directly; otherwise use a register temp. *)
  fun genTransferTarget labExp = (case labExp
       of T.LABEL _ => (labExp, [])
	| _ => let 
	      val tgtReg = newReg ()
	      in
	         (regTree(tgtReg), [move (tgtReg, labExp)]) 
	      end)

  (* jump to target *)
  fun genJump (target, labels, paramRegs, argRegs) = 
      Copy.copy {src=argRegs, dst=paramRegs} @ [T.JMP (target, labels)]

  fun genGoto varDefTbl (l, args) = let
      val name = LabelCode.getName l
      val params = LabelCode.getParamRegs l
      val args' = List.map (VarDef.useDefOf varDefTbl) args
      in
         Copy.copy {src=args', dst=params} @ [T.JMP (T.LABEL name, [name])]
      end

  (* given an offset for a parameter, return its location in scratch space *)
  fun scratchLoc offset = T.ADD(MTy.wordTy, offset, T.REG(MTy.wordTy, Regs.argReg))

  (* convert a finalized location to a MLRISC tree *) 
  fun locToTree (ty, SA.REG (ty', r), K_GPR) = 
      if (ty' < ty)
         then MTy.EXP (ty, T.ZX (ty, ty', T.REG (ty', r)))           (* type conversion is necessary *)
         else MTy.GPR (ty, r)
    | locToTree (ty, SA.REG (ty', r), K_FPR) = MTy.FPR (ty, r)
    (* effective address of the block offset *)
    | locToTree (ty, SA.BLOCK_OFFSET off, _) = MTy.EXP (ty, scratchLoc (T.LI (T.I.fromInt(MTy.wordTy, off))))
    | locToTree (ty, SA.NARROW (loc, ty', k), _) = locToTree(ty', loc, k)

  (* copy an argument to a parameter location *)
  fun copyArgToParam (param, arg) = (case (param, arg)
      of (MTy.GPR (ty, r), _) => Copy.copy {dst=[MTy.GPReg(ty, r)], src=[arg]}
       | (MTy.FPR (ty, r), _) => Copy.copy {dst=[MTy.FPReg(ty, r)], src=[arg]}
       (* put the argument in scratch space *)
       | (MTy.EXP (ty, offset), MTy.EXP (_, e)) => [T.STORE (ty, offset, e, ManticoreRegion.memory)]
       | (MTy.EXP (ty, offset), MTy.FEXP (_, e)) => [T.FSTORE (ty, offset, e, ManticoreRegion.memory)])

  (* discard non-register trees *)
  fun treeToReg (MTy.GPR (ty, r), regs) = MTy.GPReg (ty, r) :: regs
    | treeToReg (MTy.FPR (ty, r), regs) = MTy.FPReg (ty, r) :: regs
    | treeToReg (_, regs) = regs

  (* filter out those MLRISC trees that are not registers *)
  fun treesToRegs mlts = List.rev (List.foldl treeToReg [] mlts)

  fun genStdTransfer varDefTbl (target, args, params) = let
      val getDefOf = VarDef.getDefOf varDefTbl
      val argTmpRegs = List.map newReg args
      in
	  {stms=List.concat [	   
	   (* copy arguments into temp registers;  this breaks up the live ranges, which 
	    * should help the register allocator.
	    *)
	   Copy.copy {dst=List.map (fn r => MTy.GPReg (MTy.wordTy, r)) argTmpRegs, src=List.map getDefOf args},
	   Copy.copy {dst=params, src=List.map (fn r => MTy.GPR (MTy.wordTy, r)) argTmpRegs},
	   (* jump to the target *)
	   [T.JMP (target, [])]],
	   liveOut=List.map (T.GPR o regTree o (fn MTy.GPReg (_, r) => r)) params}
      end

  fun kindOfCFGTy cfgTy = (case MTy.cfgTyToMLRisc cfgTy
      of MTy.K_INT => K_GPR
       | MTy.K_COND => K_GPR
       | MTy.K_FLOAT => K_FPR)

  (* convert an argument to a location for staged allocation *)
  fun tyToLoc ty = (Types.szOf ty, kindOfCFGTy ty, MTy.wordTy div 8)

  (* determine the staged-allocation locations for some parameter types *)
  fun paramLocations (str0, step, paramSlots) = let
      fun finalizeLoc (slot, (str, paramLocs)) = let
	  val (str, slot) = step(str, slot)
          in
             (str, slot :: paramLocs)
          end
      val (str, paramLocs) = List.foldl finalizeLoc (str0, []) paramSlots
      in
         (str, List.rev paramLocs)
      end

  fun genStdApply varDefTbl {f, clos, args as [arg], ret, exh} = let
      val defOf = VarDef.defOf varDefTbl 
      val args = [clos, arg, ret, exh] 
      (* generate the finite automata for generating the call *)
      val {c0, cStep, finish} = CallingConventions.stdApply()
      (* determine the destinations of parameters (all should be in gprs) *)
      val paramSlots = List.map (fn _ => (MTy.wordTy, K_GPR, MTy.wordTy div 8)) args
      val (_, paramLocs) = paramLocations(c0, cStep, paramSlots)
      val params = List.map locToTree paramLocs
      val paramRegs = treesToRegs params
      val (lab, mvInstr) = genTransferTarget (defOf f)
      val {stms, liveOut} = genStdTransfer varDefTbl (lab, args, paramRegs) 
      in
          {stms=mvInstr @ stms, liveOut=liveOut}
      end 
    | genStdApply _ _ = raise Fail "genStdApply: ill-formed StdApply"  

  fun genStdThrow varDefTbl {k, clos, args as [arg]} = let
      val defOf = VarDef.defOf varDefTbl
      val args = [clos, arg]
      (* generate the finite automata for generating the call *)
      val {c0, cStep, finish} = CallingConventions.stdCont()
      (* determine the destinations of parameters (all should be in gprs) *)
      val paramSlots = List.map (fn _ => (MTy.wordTy, K_GPR, MTy.wordTy div 8)) args
      val (_, paramLocs) = paramLocations(c0, cStep, paramSlots)
      val params = List.map locToTree paramLocs
      val paramRegs = treesToRegs params
      val (labK, mvInstr) = genTransferTarget (defOf k)
      val {stms, liveOut} = genStdTransfer varDefTbl (labK, args, paramRegs)
      in 
	  {stms=mvInstr @ stms, liveOut=liveOut}
      end 
    | genStdThrow _ _ = raise Fail "genStdThrow: ill-formed StdThrow"

  fun genApply varDefTbl {f, clos, args} = let
      val defOf = VarDef.defOf varDefTbl
      val getDefOf = VarDef.getDefOf varDefTbl
      val args = clos :: args
      (* generate the finite automata for generating the call *)
      val {c0, cStep, finish=getNumScratchArgs} = CallingConventions.apply()
      val locs = List.map (tyToLoc o Var.typeOf) args
      (* determine the destinations of parameters (all should be in gprs) *)
      val (str, paramLocs) = paramLocations(c0, cStep, locs)
      val params = List.map locToTree paramLocs
      val (target, mvInstr) = genTransferTarget (defOf f)
      in
        if (getNumScratchArgs str > 0)
           then raise Fail "todo: allocate scratch space for extra arguments"
           else {stms=List.concat [
		 mvInstr,
		 List.concat (ListPair.map copyArgToParam (params, List.map getDefOf args)),
		 [T.JMP (target, [])]],
		 liveOut=List.map MTy.gprToExp (treesToRegs params)}
      end

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
	  | _ => raise Fail(concat["cfgTyToCTy(", CFGTyUtil.toString ty, ")"])
	(* end case *))

  fun cvtCTy (CFunctions.PointerTy) = CTy.C_PTR
    | cvtCTy (CFunctions.BaseTy rTy) = rawTyToCTy rTy
    | cvtCTy (CFunctions.VoidTy) = CTy.C_void
				   
  (* remove a register from a list of registers *)
  fun removeReg reg regs = List.filter (not o (fn r => CellsBasis.sameCell(reg, r))) regs

  val szOfVar = Types.szOf o Var.typeOf

  fun ccall {lhs, name, retTy, paramTys, cArgs, backupRegs, saveAllocationPointer} = let
      val conv = if backupRegs then "ccall-bare" else "ccall"
      val {callseq, result} = CCall.genCall {
	    name=name, 
	    args=cArgs,
	    proto={conv=conv, retTy=retTy, paramTys=paramTys},
	    paramAlloc=fn _ => false,
	    structRet=fn _ => T.REG (64, Regs.retReg),
	    saveRestoreDedicated=fn _ => {save=[], restore=[]},
	    callComment=NONE
	  } 
      (* for each caller-save register, allocate a fresh temporary and generate save/restore operations that
       * copy the dedicated registers to/from the temporaries.
       *)
      fun saveRegs rs = let
	  fun loop ([], (tmps, ss)) = {
	        saves=T.COPY (MTy.wordTy, tmps, ss),
		restores=T.COPY (MTy.wordTy, ss, tmps)
	      }
	    | loop (r :: rs, (tmps, ss)) = loop (rs, (newReg () :: tmps, r :: ss))
          in
	    loop (rs, ([], []))
          end (* saveRegs *)
      val {saves, restores} = if saveAllocationPointer
          then saveRegs (removeReg Regs.apReg Regs.saveRegs)
          else saveRegs Regs.saveRegs
      (* we need a pointer to the vproc to set the inManticore flag, etc. *)
      val (vpReg, setVP) = hostVProc ()
      (* generate a statement to store a value in the vproc inManticore flag *)
      fun setInManticore value = 
	  VProcOps.genVPStore' (MTy.wordTy, Spec.ABI.inManticore, vpReg, T.LI value)
      (* statements to save/restore the allocation pointer from the vproc *)
      val (saveAP, restoreAP) = if saveAllocationPointer
    	  then let
             val apReg = T.REG(MTy.wordTy, Regs.apReg)
	     val save = VProcOps.genVPStore' (MTy.wordTy, Spec.ABI.allocPtr, vpReg, apReg)
	     val restore = T.MV(MTy.wordTy, Regs.apReg,	VProcOps.genVPLoad' (MTy.wordTy, Spec.ABI.allocPtr, vpReg))
             in
	        ([save], [restore])
             end
           else ([], []) 

      val stms = setVP :: saveAP @ [setInManticore(Spec.falseRep), saves]
		 @ callseq
		 @ restores :: restoreAP @ [setInManticore(Spec.trueRep)]
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
     (* check if the C function might allocate *)
      val allocates = CFunctions.protoHasAttr CFunctions.A_alloc cProtoTy
      in
        ccall {lhs=lhs, 
	       name=VarDef.defOf varDefTbl f, 
	       retTy=cvtCTy retTy, paramTys=List.map cvtCTy paramTys, 
	       cArgs=List.map (varToCArg varDefTbl) args, backupRegs=allocates, saveAllocationPointer=allocates}
      end (* genCCall *)
      
  (* Promote an object to the global heap. *)
  fun genPromote varDefTbl {lhs, arg} = 
      ccall {lhs=[lhs], 
	     name=T.LABEL RuntimeLabels.promote,
	     retTy=CTy.C_PTR, paramTys=[CTy.C_PTR, CTy.C_PTR], 
	     cArgs=[CCall.ARG VProcOps.genHostVP', varToCArg varDefTbl arg], backupRegs=false, saveAllocationPointer=true}

  (* Take the CFG variables for the GC roots and return MLRISC code that initializes and restores
   * the roots and also return the root pointer, register temps for the roots, and values for the roots
   *)
  fun processGCRoots varDefTbl (roots, restoreLoc) = let
     (* preprocess the roots *)
      fun loop ([], tys, args, temps) = (List.rev tys, List.rev args, List.rev temps)
	| loop (root::roots, tys, args, temps) = let
	   val ty = Var.typeOf root
	   val arg = VarDef.getDefOf varDefTbl root
	   val temp = varToFreshReg root 
	   in
	      loop (roots, ty::tys, arg::args, temp::temps)
	   end
     (* types, values, and temporary registers for the roots (order matters) *)
      val (rootTys, rootArgs, rootTemps) = loop (roots, [], [], [])
     (* allocate the roots *)
      val {ptr=rootPtr, stms=initRoots} = Alloc.genAlloc (ListPair.zip (rootTys, List.map MTy.regToTree rootTemps))
     (* restore the roots *)
      fun restore ([], i, rs) = List.rev rs
	| restore (ty::tys, i, rs) = let
            val r = Alloc.select {lhsTy=ty, mty=M.T_Tuple (false, rootTys), i=i, base=restoreLoc}
	    in
	      restore (tys, i+1, r::rs)
	    end
      val restoredRoots = restore (rootTys, 0, [])
      in
         {initRoots=initRoots, restoredRoots=restoredRoots, rootPtr=rootPtr, rootTemps=rootTemps, rootArgs=rootArgs}
      end (* processGCRoots *)

  fun genAllocCCall' varDefTbl (lhs, fLabel, CFunctions.CProto(retTy, paramTys, _), fArgs, retFun, roots) = let
      val retLabel = LabelCode.getName retFun
      val retParams = LabelCode.getParamRegs retFun
     (* pre-C-Call vproc pointer *)
      val (vpReg, setVP) = hostVProc ()
     (* post-C-call vproc pointer*)
      val (vpReg', setVP') = hostVProc ()
      val stdEnvPtrOffset = VProcOps.genVPAddrOf (Spec.ABI.stdEnvPtr, vpReg')
      val {initRoots, restoredRoots, rootPtr, rootTemps, rootArgs} = 
	     processGCRoots varDefTbl (roots, T.LOAD (MTy.wordTy, stdEnvPtrOffset, ManticoreRegion.memory))
     (* if the C call returns a value (it is non-void), we pass the return value to the first argument 
      * position of the return function (ret) 
      *)      
      val {stms=stmsC, result=resultC} = ccall {lhs=lhs, name=fLabel, 
               retTy=cvtCTy retTy, paramTys=List.map cvtCTy paramTys, 
	       cArgs=fArgs, backupRegs=true, saveAllocationPointer=false}
      in
         List.concat [
	    Copy.copy {dst=rootTemps, src=rootArgs},
	    initRoots,
	    [setVP],
           (* store dedicated registers and the rootset pointer before entering the C call *)
	    [ (* by convention we put the rootset pointer into stdEnvPtr *)
              VProcOps.genVPStore' (MTy.wordTy, Spec.ABI.stdEnvPtr, vpReg, MTy.mlriscTreeToRexp rootPtr),
	      VProcOps.genVPStore' (MTy.wordTy, Spec.ABI.allocPtr, vpReg, regTree Regs.apReg),
	      VProcOps.genVPStore' (MTy.wordTy, Spec.ABI.limitPtr, vpReg, regTree Regs.limReg) 
	    ],
           (* call the C function *)
            stmsC,	    
	   (* restore dedicated registers *) 
	    [
	     setVP',
	     move (Regs.apReg, (VProcOps.genVPLoad' (MTy.wordTy, Spec.ABI.allocPtr, vpReg'))),
	     move (Regs.limReg, (VProcOps.genVPLoad' (MTy.wordTy, Spec.ABI.limitPtr, vpReg')))
	    ],
	   (* jump to the return post-C-call function *)
	    genJump (T.LABEL retLabel, [retLabel], retParams, resultC @ restoredRoots)
	 ]
      end (* genAllocCCall' *)

 (* apply a C funcion f to args (f can trigger a garbage collection). *)
  fun genAllocCCall varDefTbl {lhs, f, args, ret=ret as (retFun, roots)} = let
      val fLabel = VarDef.defOf varDefTbl f
      val fPrototype = getCPrototype f
      val fArgs = List.map (varToCArg varDefTbl) args
      in
        genAllocCCall' varDefTbl (lhs, fLabel, fPrototype, fArgs, retFun, roots)
      end

  fun moveMLTree (r, mlt) = (case MTy.treeToMLRisc mlt
	of T.GPR e => move (r, e))

  (* Generate a local heap check.
   *
   * We check if it is necessary to transfer control into the runtime system.  This can happen for
   * two reasons: there is insufficent space in the local heap (obvious) or the vproc received
   * a preemption.  In the latter case, the runtime might execute other allocating code, and thus the 
   * heap could have insufficient space after returning from the runtime system.
   *
   * The heap check works as follows. If the check succeeds, apply the nogc function.  Otherwise, 
   * transfer into the runtime system by doing these steps:
   *   1. Allocate the root set in the heap (we dedicate heap slop space to ensure this will work).
   *   2. Put the root set pointer into the closure register, and put the return continuation into the 
   *      return register.
   *   3. Call the GC initialization routine passing it the return continuation and the root set.
   *
   * The return continuation is somewhat unusual, as it jumps right back to the heap limit test.  It 
   * must do this step for the reason mentioned earlier: the heap might still have insufficient space
   * because of preemption.
   *)
  fun genHeapCheck varDefTbl {hck=CFG.HCK_Local, szb, nogc=(noGCLab, roots)} = let
      val {initRoots, restoredRoots, rootPtr, rootTemps, rootArgs } = processGCRoots varDefTbl (roots, regTree Regs.closReg)
								      
      val noGCParamRegs = LabelCode.getParamRegs noGCLab
      val noGCLab = LabelCode.getName noGCLab
      val retLab = newLabel "retGC"
      val gcTestLab = newLabel "gcTest"
		      
     (* perform the GC *)
      val doGCLab = newLabel "doGC"
      val doGCStms = List.concat [
 	      [T.DEFINE doGCLab],	      
	     (* allocate a heap object for GC roots *)
	      initRoots,
	     (* save the root pointer in the closure register *)
	      [moveMLTree (Regs.closReg, rootPtr)],
	     (* put the return address into retReg *)
	      [move (Regs.retReg, T.LABEL retLab)],
	     (* jump to the garbage collector *)
	      Target.genGCCall () 
          ]

     (* jump to the heap limit check *)
      val retStms = genJump (T.LABEL gcTestLab, [gcTestLab], rootTemps, restoredRoots)

      val stms = List.concat [
                 (* force the root set into registers *)
		  Copy.copy {dst=rootTemps, src=rootArgs},
		  [T.DEFINE gcTestLab],	      
		 (* branch on the heap limit test *)
		  [T.BCC (Alloc.genAllocCheck szb, doGCLab)],
		 (* GC is unnecessary *)
		  genJump (T.LABEL noGCLab, [noGCLab], noGCParamRegs, List.map MTy.regToTree rootTemps),
		 (* GC is necessary *)
		  doGCStms 
             ] 
      in	  
	  {stms=stms, return=SOME (retLab, retStms, List.map MTy.gprToExp noGCParamRegs)}
      end
   (* Generate a global heap check.
    *
    * If there is insufficient space in the global heap, allocate a memory chunk in the run-time system.
    *
    *  if (globNextW + szB > globLimit) {
    *     GetChunkForVProc (host_vproc);
    *  }
    *  noGCRoots (roots)
    *
    *)
    | genHeapCheck varDefTbl {hck=CFG.HCK_Global, szb, nogc} = let
      val getChunkLab = newLabel "getChunk"
      val {stms=getGlobalChunkStms, ...} = 
	  ccall {lhs=[], name=T.LABEL RuntimeLabels.getGlobalChunk,
	     retTy=CTy.C_void, paramTys=[CTy.C_PTR], 
	     cArgs=[CCall.ARG VProcOps.genHostVP'], backupRegs=false, saveAllocationPointer=true}
      val continueStms = genGoto varDefTbl nogc
     (* Call into the runtime system to allocate a global heap chunk. *)
      val getChunkStms = List.concat [
			 [T.DEFINE getChunkLab],
			 getGlobalChunkStms,
			 continueStms
	  ]
      val {stms=allocCheckStms, allocCheck} = Alloc.genGlobalAllocCheck szb
     (* Check that there is sufficient space in the global heap. *)
      val chkStms = List.concat [
		    allocCheckStms,
		    [T.BCC (allocCheck, getChunkLab)],
		    continueStms
          ]
      in
	  {stms=chkStms @ getChunkStms, return=NONE}
      end (* genHeapCheck *)

  (* bind a parameter *)
  fun bindParam (param, k) = (case (param, k)
      of (MTy.GPR (ty, r), K_GPR) => Copy.fresh [MTy.GPReg(ty, r)]
       | (MTy.FPR (ty, r), K_FPR) => Copy.fresh [MTy.GPReg(ty, r)]
       (* load the parameter from scratch space *)
       | (MTy.EXP (ty, offset), K_GPR) => let
	  val tmp = newReg()
          in
	      {stms=[T.MV (ty, tmp, T.LOAD(ty, offset, ManticoreRegion.memory))],
	       regs=[MTy.GPReg(ty, tmp)]}
          end
       (* load the parameter from scratch space *)
       | (MTy.EXP (ty, offset), K_FPR) => let
	  val tmp = newFReg()
          in
	      {stms=[T.FMV (ty, tmp, T.FLOAD(ty, offset, ManticoreRegion.memory))],
	       regs=[MTy.FPReg(ty, tmp)]}
          end)

  (* bind parameters to fresh, local registers *)
  fun bindParams {params, paramKinds} = let
      fun f (param, k, {stms, regs}) = let
	  val {stms=stms', regs=regs'} = bindParam (param, k)
          in
	     {stms=stms'@stms, regs=regs'@regs}
          end
      val {stms, regs} = ListPair.foldl f {stms=[], regs=[]} (params, paramKinds)
      in
         {stms=stms, regs=List.rev regs}
      end

  fun setEntry varDefTbl (lab, locals, args, assignStms) = (
        (* make param registers available globally *)
        LabelCode.setParamRegs (lab, locals);
	(* bind params to their registers *)
	ListPair.app (VarDef.setDefOf varDefTbl) (args, List.map MTy.regToTree locals);
	assignStms)

 (* generate the entry code for a function *)
  fun genFuncEntry varDefTbl (lab, conv as M.StdFunc {clos, args as [arg], ret, exh}) = let
      val args = [clos, arg, ret, exh]
      (* generate the finite automata for generating the call *)
      val {c0, cStep, finish} = CallingConventions.stdApply()
      val paramSlots = List.map (fn _ => (MTy.wordTy, K_GPR, MTy.wordTy div 8)) args
      (* determine the destinations of parameters (all should be in gprs) *)
      val (_, paramLocs) = paramLocations(c0, cStep, paramSlots)
      val params = List.map locToTree paramLocs
      val {stms, regs} = Copy.fresh (treesToRegs params)
      in
          setEntry varDefTbl (lab, regs, args, stms)
      end
    | genFuncEntry varDefTbl (lab, conv as M.StdCont {clos, args as [arg]}) = let
      val args = [clos, arg]
      (* generate the finite automata for generating the call *)
      val {c0, cStep, finish} = CallingConventions.stdCont()
      val paramSlots = List.map (fn _ => (MTy.wordTy, K_GPR, MTy.wordTy div 8)) args
      (* determine the destinations of parameters (all should be in gprs) *)
      val (_, paramLocs) = paramLocations(c0, cStep, paramSlots)
      val params = List.map locToTree paramLocs
      val {stms, regs} = Copy.fresh (treesToRegs params)
      in
          setEntry varDefTbl (lab, regs, args, stms)
      end
    | genFuncEntry varDefTbl (lab, conv as M.KnownFunc {clos, args}) = let
      val args = clos :: args
      (* generate the finite automata for generating the call *)
      val {c0, cStep, finish=getNumScratchArgs} = CallingConventions.apply()
      val paramSlots = List.map (tyToLoc o Var.typeOf) args
      (* determine the destinations of parameters *)
      val (str, paramLocs) = paramLocations(c0, cStep, paramSlots)
      val params = List.map locToTree paramLocs
      val {stms, regs} = bindParams {params=params, paramKinds=List.map #3 paramLocs}
      in
	  setEntry varDefTbl (lab, regs, args, stms)
      end
    | genFuncEntry varDefTbl (lab, conv as M.Block {args}) = 
      setEntry varDefTbl (lab, List.map varToFreshReg args, args, [])

end (* HeapTransferFn *)
