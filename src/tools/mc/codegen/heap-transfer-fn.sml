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

  fun newLabel s = Label.label s () 
  fun fail s = raise Fail s
  fun regExp r = T.REG (ty, r)
  fun move (r, e) = T.MV (ty, r, e)
  fun move' (r, mlt) = 
      (case MTy.treeToMLRisc mlt
	of T.GPR e => move (r, e)
	 | _ => fail "move'"
      (* esac *))
  fun newReg _ = Cells.newReg ()
  fun newFReg _ = Cells.newFreg ()
  fun gpReg r = MTy.GPReg (ty, r)
  fun mltGPR r = MTy.GPR (ty, r)
  fun regGP (MTy.GPReg (_, r)) = r
  fun mkExp rexp = MTy.EXP (ty, rexp)
  val toGPR = T.GPR o regExp o regGP
  fun mlrReg v = 
      let val mty = Var.typeOf v
      in
	  (case MTy.cfgTyToMLRisc mty
	    of MTy.K_INT => MTy.GPReg (Types.szOf mty, newReg ())
	     | MTy.K_FLOAT => MTy.FPReg (Types.szOf mty, newFReg ())
	  (* esac *))
      end (* mlrReg *)

  fun select' (lhsTy, mty, i, e) = 
      Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=e}
  fun select {lhsTy, mty, i, base} =
      (case Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=base}
	of MTy.EXP (_, e) => e
	 | _ => raise Fail ""
      (* esac *))

  (* emit a jump by first copying argument registers to parameter registers
   * and then emitting a jump instruction. *)
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

  (* if labExp is a label, put it into the jump directly, ow use a 
   * register temp. *)
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
	  | Ty.T_Wrap _ => CTy.C_PTR
	  | Ty.T_Tuple _ => CTy.C_PTR
	  | Ty.T_OpenTuple _ => CTy.C_PTR
	  | Ty.T_VProc => CTy.C_PTR
	  | _ => raise Fail(concat["cfgTyToCTy(", CFGTy.toString ty, ")"])
	(* esac *))

    fun cvtCTy (CFunctions.PointerTy) = CTy.C_PTR
      | cvtCTy (CFunctions.BaseTy rTy) = rawTyToCTy rTy
      | cvtCTy (CFunctions.VoidTy) = CTy.C_void

  (* remove a register from a list of registers *)
    fun removeReg reg (r::rs) = if CellsBasis.sameCell(reg, r)
	  then rs
	  else removeReg reg rs
      | removeReg _ [] = []

    fun genCCall varDefTbl {frame, lhs, f, args} = let
	  val defOf = VarDef.defOf varDefTbl
	  val getDefOf = VarDef.getDefOf varDefTbl
	(* get the C function's prototype *)
	  val cProtoTy as CFunctions.CProto(retTy, paramTys, _) = (
		case Var.typeOf f
		 of CFGTy.T_CFun proto => proto
		  | _ => raise Fail(concat["genCCall: ", Var.toString f, " not a C function"])
		(* end case *))
	(* convert from CFunctions.c_type to CTypes.c_type *)
	  val retTy = cvtCTy retTy
	  val paramTys = List.map cvtCTy paramTys
	  val szOfVar = Types.szOf o Var.typeOf
	  val name = defOf f
	  fun mlriscToCArg (T.GPR rexp) = CCall.ARG rexp
	    | mlriscToCArg (T.FPR fexp) = CCall.FARG fexp
	    | mlriscToCArg _ = raise Fail "impossible"
	  val cArgs = map (mlriscToCArg o MTy.treeToMLRisc o getDefOf) args
	  val {callseq, result} = 
	      CCall.genCall {
		  name=name, 
		  args=cArgs,
		  proto={conv="", retTy=retTy, paramTys=paramTys},
		  paramAlloc=fn _ => false,
		  structRet=fn _ => T.REG (64, retReg),
		  saveRestoreDedicated=fn _ => {save=[], restore=[]},
		  callComment=NONE
		} 
	(* do we need to save/restore the allocation pointer? *)
	  val saveAllocPtr = CFunctions.protoHasAttr CFunctions.A_alloc cProtoTy
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
	  val (vpReg, setVP) = let
		val r = newReg()
		val MTy.EXP(_, hostVP) = VProcOps.genHostVP
		in
		  (T.REG(ty, r), T.MV(ty, r, hostVP))
		end
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
	  end (* genCCall *)
      
  (* Check whether the heap contains szb free bytes. If it does, apply the
   * nogc function.  Otherwise, perform the GC with the following steps:
   * 1. Allocate the root set (roots) in the heap-slop space (assume
   *    there is ample space).
   * 2. Put the root set pointer into closReg, and put the return address,
   *    retKLbl, into retReg.
   * 3. Call the GC initialization routine, passing it retK in the return 
   *    continuation register.
   *)
  fun genHeapCheck varDefTbl {szb, nogc=(noGCLbl, roots)} =
      let fun argInfo ([], argTys, hcArgs, mlRegs) = 
	      (rev argTys, rev hcArgs, rev mlRegs)
	    | argInfo (a :: args, argTys, hcArgs, rootsMLR) =
	      argInfo (args, Var.typeOf a :: argTys, 
		       VarDef.getDefOf varDefTbl a :: hcArgs,
		       mlrReg a :: rootsMLR)
	  (*   arg type,  arg,     fresh arg register *)
	  val (argTys,    args,    rootsMLR) = argInfo (roots, [], [], [])
	      
	  val noGCParamRegs = LabelCode.getParamRegs noGCLbl
	  val noGCLbl = LabelCode.getName noGCLbl
	  val retKLbl = newLabel "retGCK"

	  val rootSet = ListPair.zip (argTys, map MTy.regToTree rootsMLR)
	  (* heap allocate the root set *)
	  val {ptr=rootReg, stms=allocStms} = Alloc.genAlloc rootSet

	  (* perform the GC *)
	  val doGCLbl = newLabel "doGC"
	  val doGCStms = List.concat [
 	      [T.DEFINE doGCLbl],
	      (* force the root set into registers *)
	      Copy.copy {dst=rootsMLR, src=args},
	      (* allocate a heap object for GC roots *)
	      allocStms,
	      (* save the root pointer in the closure register *)
	      [move' (closReg, rootReg)],
	      (* put the return address into retReg *)
	      [move (retReg, T.LABEL retKLbl)],
	      (* perform the GC *)
	      Target.genGCCall () ]

	  (* load the fresh roots that GC produces *)
	  fun loadArgs ([], i, ss) = rev ss
	    | loadArgs (mty :: mtys, i, ss) =
	      let val ty = Types.szOf mty
		  val s = select' (ty, M.T_Tuple (false, argTys), i, regExp closReg)
	      in 
		  loadArgs (mtys, i + 1, s :: ss)
	      end
	  val restoredRoots = loadArgs (argTys, 0, [])
	  (* generate the return continuation from GC *)
	  val retKStms = (* jump to the post-gc function *)
	      genJump (T.LABEL noGCLbl, [noGCLbl], noGCParamRegs, restoredRoots)

	  (* if the allocation check succeeds (there is sufficient heap space),
	   * apply noGCLbl.  otherwise, perform the GC. *)
	  val stms = List.concat [
	      [T.BCC (Alloc.genAllocCheck szb, doGCLbl)],
	      genJump (T.LABEL noGCLbl, [noGCLbl], noGCParamRegs, args),
	      doGCStms ] 
      in	  
	  {stms=stms, retKLbl=retKLbl, retKStms=retKStms, liveOut=map MTy.gprToExp noGCParamRegs}
      end (* genHeapCheck *)

  fun genFuncEntry varDefTbl (lab, convention) =
      let datatype conv = Special | StdConv of Regs.gpr list
	  val (params, stdRegs) = (case convention
		of M.StdFunc{clos, args as [arg], ret, exh} => 
		   ([clos, arg, ret, exh], StdConv stdFuncRegs)
                 | M.StdFunc _ => raise Fail "genFuncEntry: ill-formed StdFunc"
		 | M.StdCont{clos, args as [arg]} => 
                   ([clos, arg], StdConv stdContRegs)
                 | M.StdCont _ => raise Fail "genFuncEntry: ill-formed StdCont"
                 | M.KnownFunc args =>
                   (args, StdConv (List.take (kfncRegs, length args)))
		 | M.Block vs => (vs, Special)
	      (* esac *))
	  fun bindToParams rs = 
	      ListPair.app (VarDef.setDefOf varDefTbl) (params, rs)
	  fun gpReg' (v, r) = MTy.GPReg (Types.szOf (Var.typeOf v), r)
	  val {stms, regs} = 
	      (case stdRegs
		of Special => (* specialized calling convention or block *)
		   {stms=[], regs=map mlrReg params}
		 | StdConv stdRegs => (* standard calling convention *) 
		   Copy.fresh (ListPair.map gpReg' (params, stdRegs))
	      (* esac *))
      in 	
	  LabelCode.setParamRegs (lab, regs);
	  bindToParams (map MTy.regToTree regs);
	  stms
      end (* genFuncEntry *)

end (* HeapTransferFn *)
