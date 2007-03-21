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
) : TRANSFER = struct

  structure MTy = MTy
  structure T = MTy.T
  structure VarDef = VarDef
  structure SpillLoc = SpillLoc
  structure Cells = MLTreeComp.I.C
  structure M = CFG
  structure Var = M.Var
  structure Frame = Frame

  val apReg = Regs.apReg
  val wordSzB = Word.toInt Spec.C.wordSzB
  val wordAlignB = Word.toInt Spec.C.wordAlignB
  val ty = MTy.wordTy
  val iTy = Types.szOf (CFGTy.T_Raw CFGTy.T_Int)
  val memory = ManticoreRegion.memory

  val stdCallRegs as [closReg, argReg, retReg, exhReg] = 
      [Regs.closReg, Regs.argReg, Regs.retReg, Regs.exhReg]
  val stdContRegs as [closReg, argReg] = 
      [Regs.closReg, Regs.argReg]

  fun newLabel s = Label.label s () 
  fun fail s = raise Fail s
  fun intLit i = T.LI (T.I.fromInt (ty, i))
  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
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

  fun bind varDefTbl (x, T.GPR e) = 
      VarDef.bind varDefTbl (Types.szOf (Var.typeOf x), x, e)
    | bind varDefTbl (x, T.FPR e) = 
      VarDef.fbind varDefTbl (Types.szOf (Var.typeOf x), x, e)

  fun select' (lhsTy, mty, i, e) = 
      Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=e}
  fun select {lhsTy, mty, i, base} =
      (case Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=base}
	of MTy.EXP (_, e) => e
	 | _ => raise Fail ""
      (* esac *))

  fun genJump (target, ls, params, args) =
      let val stms = Copy.copy {src=args, dst=params}
      in
	  stms @ [T.JMP (target, ls)]
      end (* genJump *)

  fun genGoto varDefTbl (l, args) =
      let val getDefOf = VarDef.getDefOf varDefTbl
	  val name = LabelCode.getName l
	  val params = LabelCode.getParamRegs l
	  val args' = map getDefOf args
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
	   genJump (tgtReg, [] (* FIXME *), stdRegs, map mltGPR argRegs)],
	   liveOut=map toGPR stdRegs}
      end (* genStdTransfer *)

  fun genStdCall varDefTbl {f, clos, arg, ret, exh} = 
      let val defOf = VarDef.defOf varDefTbl
	  val args = [clos, arg, ret, exh]
	  val argRegs = map newReg args
	  val tgtReg = newReg ()
	  val {stms, liveOut} =
	      genStdTransfer varDefTbl (regExp tgtReg, args, argRegs, stdCallRegs)
      in
(* TODO: if f is a label, put it in directly, but otherwise use a temp *)
	  {stms=move (tgtReg, defOf f) :: stms, liveOut=liveOut}
      end (* genStdCall *)

  fun genStdThrow varDefTbl {k, clos, arg} = 
      let val defOf = VarDef.defOf varDefTbl
	  val kReg = newReg ()
	  val argRegs = map newReg [clos, arg]
	  val {stms, liveOut} = 
	      genStdTransfer varDefTbl (regExp kReg, [clos, arg], argRegs, stdContRegs)
      in 
	  {stms=move (kReg, defOf k) :: stms, liveOut=liveOut}
      end (* genStdThrow *)

(* generate a transfer to the "run" scheduler operation.  We pass the fiber to
 * run in the standard closure register and the signal action in the standard
 * argument register.
 *)
  fun genRun varDefTbl {act, fiber} = let
	val argRegs = map newReg [act, fiber]
	val runL = T.LABEL RuntimeLabels.run
	in
	  genStdTransfer varDefTbl (runL, [fiber, act], argRegs, stdContRegs)
	end

(* generate a transfer to the "forward" scheduler operation.  We pass the signal
 * that is being forwarded in the standard argument register.
 *)
  fun genForward varDefTbl sign = let
	val argRegs = [newReg sign]
	val forwardL = T.LABEL RuntimeLabels.forward
	in
	  genStdTransfer varDefTbl (forwardL, [sign], argRegs, [argReg])
	end

  structure Ty = CFGTy
  structure CTy = CTypes

  fun rawTyToCTy Ty.T_Byte = CTy.C_signed CTy.I_char
    | rawTyToCTy Ty.T_Short = CTy.C_signed CTy.I_short
    | rawTyToCTy Ty.T_Int = CTy.C_signed CTy.I_int
    | rawTyToCTy Ty.T_Long = CTy.C_signed CTy.I_long_long
    | rawTyToCTy Ty.T_Float = CTy.C_float
    | rawTyToCTy Ty.T_Double = CTy.C_double
    | rawTyToCTy Ty.T_Vec128 = raise Fail "todo"

  fun cfgTyToCTy ty =
      (case ty
	of Ty.T_Any => CTy.C_PTR
	 | Ty.T_Raw rt => rawTyToCTy rt
	 | Ty.T_Enum _ => CTy.C_signed CTy.I_int
	 | Ty.T_Wrap _ => CTy.C_PTR
	 | Ty.T_Tuple _ => CTy.C_PTR
	 | Ty.T_OpenTuple _ => CTy.C_PTR
	 | _ => raise Fail "cfgTyToCTy"
      (* esac *))

  fun genCCall varDefTbl {frame, lhs, f, args} =
      let val defOf = VarDef.defOf varDefTbl
	  val getDefOf = VarDef.getDefOf varDefTbl
	  val getTy = cfgTyToCTy o Var.typeOf
	  val szOfVar = Types.szOf o Var.typeOf
	  val name = defOf f
	  val cArgs = map (MTy.treeToMLRisc o getDefOf) args
	  val retTy = (case lhs
			of l :: _ => getTy l
			 | [] => CTy.C_signed CTy.I_int
		       (* esac *))
	  val paramTys = map getTy args
	  val {callseq, result} = CCall.genCall {
		  name=name, args=cArgs,
		  proto={conv="", retTy=retTy, paramTys=paramTys} }
	  fun convResult (T.GPR e, v) = MTy.EXP (szOfVar v, e)
	    | convResult (T.FPR e, v) = MTy.FEXP (szOfVar v, e)
	    | convResult _ = raise Fail "convResult"
				   
	  fun saveRegs rs =	      
	      let fun loop ([], (tmps, ss)) = 
		      {saves=T.COPY (ty, tmps, ss),
		       restores=T.COPY (ty, ss, tmps)}
		    | loop (r :: rs, (tmps, ss)) =
		      loop (rs, (newReg () :: tmps, r :: ss))
	      in
		  loop (rs, ([], []))
	      end (* saveRegs *)
	  val {saves, restores} = saveRegs Regs.saveRegs
      in
(* FIXME: unset the IN_MANTICORE flag *)
	  {stms=[saves] @ callseq @ [restores], 
	   result=ListPair.map convResult (result, lhs)}
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
		  val s = select' (ty, M.T_Tuple argTys, i, regExp closReg)
	      in 
		  loadArgs (mtys, i + 1, s :: ss)
	      end
	  val restoredRoots = loadArgs (argTys, 0, [])
	  (* generate the return continuation from GC *)
	  val retK = List.concat [
 	      [T.DEFINE retKLbl],
	      (* jump to the post-gc function *)
	      genJump (T.LABEL noGCLbl, [noGCLbl], noGCParamRegs, restoredRoots) ]

	  (* if the allocation check succeeds (there is sufficient heap space),
	   * apply noGCLbl.  otherwise, perform the GC. *)
	  val stms = List.concat [
	      [T.BCC (Alloc.genAllocCheck szb, doGCLbl)],
	      genJump (T.LABEL noGCLbl, [noGCLbl], noGCParamRegs, args),
	      retK,
	      doGCStms ] 
(*	  val stms = List.concat [ doGCStms, retK ]*)
      in
	  {stms=stms, liveOut=map MTy.gprToExp noGCParamRegs}
      end (* genHeapCheck *)

  fun genFuncEntry varDefTbl (lab, convention) =
      let datatype conv = Special | StdConv of Regs.gpr list
	  val (params, stdRegs) = 
	      (case convention
		of M.StdFunc {clos, arg, ret, exh} => 
		   ([clos, arg, ret, exh], StdConv stdCallRegs)
		 | M.StdCont {clos, arg} => ([clos, arg], StdConv stdContRegs)
		 | ( M.KnownFunc vs | M.Block vs ) => (vs, Special)
	      (* esac *))
	  fun bindToParams rs = 
	      ListPair.app (bind varDefTbl) (params, rs)
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
	  bindToParams (map MTy.gprToExp regs);
	  stms
      end (* genFuncEntry *)

end (* HeapTransferFn *)
