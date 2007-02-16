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
) : TRANSFER = struct

  structure MTy = MTy
  structure T = MTy.T
  structure VarDef = VarDef
  structure SpillLoc = SpillLoc
  structure Cells = MLTreeComp.I.C
  structure M = CFG
  structure Var = M.Var

  val apReg = Regs.apReg
  val wordSzB = Word.toInt Spec.wordSzB
  val wordAlignB = Word.toInt Spec.wordAlignB
  val ty = MTy.wordTy
  val iTy = Types.szOf (CFGTy.T_Raw CFGTy.T_Int)
  val memory = ManticoreRegion.memory

  val stdCallRegs as [closReg, argReg, retReg, exhReg] = 
      [Regs.closReg, Regs.argReg, Regs.retReg, Regs.exhReg]
  val stdContRegs as [closReg, argReg] = 
      [Regs.closReg, Regs.argReg]

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
  fun offAp i = T.ADD (ty, regExp apReg, litFromInt i)
  fun newReg _ = Cells.newReg ()
  fun gpReg r = MTy.GPReg (ty, r)
  fun mltGPR r = MTy.GPR (ty, r)
  fun regGP (MTy.GPReg (_, r)) = r
    | regGP (MTy.FPReg (_, r)) = r
  fun mkExp rexp = MTy.EXP (ty, rexp)
  val toGPR = T.GPR o regExp o regGP

  fun bindToRExp' varDefTbl (ty, x, e) = 
      VarDef.setDefOf varDefTbl (x, MTy.EXP (ty, e))
  fun bindToRExp varDefTbl (x, e) = 
      bindToRExp' varDefTbl (Types.szOf (Var.typeOf x), x, e)

  fun select (lhsTy, mty, i, e) = MTy.EXP (lhsTy, 
		Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=e})

  fun genJump (target, ls, params, args) =
      let val stms = Copy.copy {src=args, dst=params}
      in
	  stms @ [T.JMP (target, ls)]
      end (* genJump *)

  fun genGoto varDefTbl (l, args) =
      let val getDefOf = VarDef.getDefOf varDefTbl
	  val name = LabelCode.getName l
	  val params = LabelCode.getParamRegs l
      in
	  genJump (T.LABEL name, [name], params, map getDefOf args)
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
	   genJump (regExp tgtReg, [] (* FIXME *), stdRegs, map mltGPR argRegs)],
	   liveOut=map toGPR stdRegs}
      end (* genStdTransfer *)

  fun genStdCall varDefTbl {f, clos, arg, ret, exh} = 
      let val defOf = VarDef.defOf varDefTbl
	  val args = [clos, arg, ret, exh]
	  val argRegs = map newReg args
	  val tgtReg = newReg ()
	  val {stms, liveOut} =
	      genStdTransfer varDefTbl (tgtReg, args, argRegs, stdCallRegs)
      in
	  {stms=move (tgtReg, defOf f) :: stms, liveOut=liveOut}
      end (* genStdCall *)

  fun genStdThrow varDefTbl {k, clos, arg} = 
      let val defOf = VarDef.defOf varDefTbl
	  val kReg = newReg ()
	  val argRegs = map newReg [clos, arg]
	  val {stms, liveOut} = 
	      genStdTransfer varDefTbl (kReg, [clos, arg], argRegs, stdContRegs)
      in 
	  {stms=
	   move (kReg, defOf k) ::
	   stms,
	   liveOut=liveOut}
      end (* genStdThrow *)
      
  fun genHeapCheck varDefTbl {szb, gc, nogc=(gcLbl, argRoots)} =
      let fun argInfo (a, (argTys, args)) = 
	      (Var.typeOf a :: argTys, VarDef.getDefOf varDefTbl a :: args)
	  val (argTys, args) = foldr argInfo ([], []) argRoots

	  val params = LabelCode.getParamRegs gcLbl
	  val gcLbl = LabelCode.getName gcLbl

	  val allocCheck = Alloc.genAllocCheck szb
	  val tmpRetReg = newReg ()
	  val freshRegs = map newReg args
	  val regStms = Copy.copy {src=args, dst=map gpReg freshRegs}
	  (* allocate space on the heap for the roots *)
	  val {ptr=rootReg, stms=allocStms} = 
	      Alloc.genAlloc (ListPair.zip (argTys, map mltGPR freshRegs))
	  fun loadArg (mty, (i, ss)) =
	      (i+1,  select (ty, M.T_Tuple argTys, i, regExp argReg) :: ss)
	  val (_, ss) = foldr loadArg (0, []) argTys
	  val selStms = Copy.copy {src=rev ss, dst=params}
      in
	  {stms=List.concat [
(* FIXME: add the heap check here *)
	  (* allocate a heap object for GC roots *)
	  regStms,
	  allocStms,
	  (* save the root pointer in the argReg (where the GC expects it) *)
	  [move' (argReg, rootReg)],
	  (* perform the GC *)
	  Target.genGCCall (),
	  (* revive the roots *)
	  selStms,
	  (* jump to the post-gc function *)
	  [T.JMP (T.LABEL gcLbl, [gcLbl])]
	  ], liveOut=map (T.GPR o regExp o regGP) params}
      end (* genHeapCheck *)

  fun genFuncEntry varDefTbl (lab, convention) =
      let datatype conv = Special | StdConv of Regs.gpr list
	  val (lhs, stdRegs) = 
	      (case convention
		of M.StdFunc {clos, arg, ret, exh} => 
		   ([clos, arg, ret, exh], StdConv stdCallRegs)
		 | M.StdCont {clos, arg} => ([clos, arg], StdConv stdContRegs)
		 | ( M.KnownFunc vs | M.Block vs ) => (vs, Special)
	      (* esac *))
	  fun bindToLHS rs = ListPair.app (bindToRExp varDefTbl) (lhs, rs)
	  fun gpReg' (v, r) = MTy.GPReg (Types.szOf (Var.typeOf v), r)
	  val {stms, regs} = 
	      (case stdRegs
		of Special => (* specialized calling convention or block *)
		   {stms=[], regs=ListPair.map gpReg' (lhs, map newReg lhs)}
		 | StdConv stdRegs => (* standard calling convention *)
		   Copy.fresh (ListPair.map gpReg' (lhs, stdRegs))
	      (* esac *))
      in 	
	  LabelCode.setParamRegs (lab, regs);
	  bindToLHS (map MTy.gprToExp regs);
	  stms
      end (* genFuncEntry *)

  (* shorthands for some CFG types *)
  val aTy = M.T_Any
  val retTy = M.T_Code [aTy]
  val kTy = M.T_OpenTuple [retTy]
  val longTy = M.T_Raw RawTypes.T_Long

  fun genModuleEntry code =
      let val entryLbl = 
	      (case code
		of M.FUNC {lab, ...} :: _ => LabelCode.getName lab
		 | _ => raise Fail ""
	      (* esac *))
	  val initLbl = "initK"

	  val {stms=argInitStms, ptr=wArgReg} = Alloc.genWrap (longTy, mltGPR argReg)
	  val {stms=initClosStms, ptr=initClosReg} = 
	      Alloc.genAlloc 
		      [(CFGTy.unitTy, mkExp (intLit 0)),
		       (aTy, mkExp (T.LABEL entryLbl))]

	  val {ptr=rReg, stms=retKStms} = 
	      Alloc.genAlloc [(retTy, mkExp (T.LABEL (Label.global "returnloc")))]
	  val {ptr=initReg, stms=initKStms} = Alloc.genAlloc 
		[(retTy, mkExp (T.LABEL (Label.global initLbl))),
		 (aTy, mltGPR closReg), 
		 (aTy, mltGPR argReg), 
		 (kTy, rReg), 
		 (kTy, mltGPR exhReg)]
	  val mty = M.T_Tuple [retTy, aTy, longTy, kTy, kTy]
	  val baseReg = newReg ()
	  val kReg = newReg ()
	  val aReg = newReg ()
	  fun doSelect (r, i) = move (r, 
	      Alloc.select {lhsTy=ty, mty=mty, i=i, base=regExp aReg} )
	  val (selects, _) = 
	      foldl (fn (r, (ss, i)) => (doSelect (r, i) :: ss, i+1)) ([], 2) 
		    [argReg, retReg]
      in
	  {modEntryLbl="mantentry",
	   entryStms=List.concat [
	   (* wrap the integer argument *)
	   argInitStms,
	   [move' (argReg, wArgReg)], 
	   (* create an initial closure *)
	   initClosStms,
	   [move' (closReg, initClosReg)],
	   (* allocate and save the outer continuation *)
	   retKStms,
	   (* allocate and save the initial continuation *)
	   initKStms,
	   [move' (retReg, initReg)],
	   (* jump to the module initialization function *)
	   genJump (T.LABEL entryLbl, [], [], [])
	   ],
	   initLbl=initLbl,
	   initStms=List.concat [
	   (* retrieve the entry point function *)
	   [move (baseReg, regExp argReg)],
	   [move (kReg, Alloc.select {lhsTy=ty, mty=mty, i=1, base=regExp baseReg})],
	   (* retrieve the outgoing argument, return cont, and exception *) 
	   [move (aReg, regExp closReg)],
	   selects,
	   (* retrieve the initial closure *)
	   [move (closReg, Alloc.select {lhsTy=ty, mty=mty, i=0, base=regExp baseReg})],
	   genJump (regExp kReg, [],  [], [])
	   ]
	  }
      end (* genModuleEntry *)

end (* HeapTransferFn *)
