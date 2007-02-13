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

  val genGCCall = Target.genGCCall

  val closReg = Regs.closReg
  val exhReg = Regs.exhReg
  val argReg = Regs.argReg
  val retReg= Regs.retReg
  val apReg = Regs.apReg
  val wordSzB = Word.toInt Spec.wordSzB
  val wordAlignB = Word.toInt Spec.wordAlignB
  val ty = wordSzB * 8
  val iTy = 32
  val memory = ManticoreRegion.memory

  val stdCallRegs = [closReg, argReg, retReg, exhReg]
  val stdContRegs = [closReg, argReg]

  fun intLit i = T.LI (T.I.fromInt (ty, i))
  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun regExp r = T.REG (ty, r)
  fun move (r, e) = T.MV (ty, r, e)
  fun offAp i = T.ADD (ty, regExp apReg, litFromInt i)
  fun newReg _ = Cells.newReg ()
  fun gpReg r = MTy.GPReg (ty, r)
  fun mltGPR r = MTy.GPR (ty, r)
  fun regGP (MTy.GPReg (_, r)) = r
    | regGP (MTy.FPReg (_, r)) = r
  fun mkExp rexp = MTy.EXP (ty, rexp)
  fun fail s = raise Fail s
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
	   (* copy the function and its args into temp registers *)
	   Copy.copy {src=map getDefOf args, 
		      dst=map gpReg argRegs},
	   (* jump to the function with its fresh args *)
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
(*	   [move (kReg, Alloc.select {lhsTy=ty, mty=M.Var.typeOf k, 
				      i=0, base=getDefOf k})],*)
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
	  val rootReg = newReg ()
	  val tmpRetReg = newReg ()
	  val freshRegs = map newReg args
	  val regStms = Copy.copy {src=args, dst=map gpReg freshRegs}
	  (* allocate space on the heap for the roots *)
	  val allocStms = 
	      Alloc.genAlloc (ListPair.zip (argTys, map mltGPR freshRegs))
	  fun loadArg (mty, (i, ss)) =
	      (i+1,  select (ty, M.T_Tuple argTys, i, regExp argReg) :: ss)
	  val (_, ss) = foldr loadArg (0, []) argTys
	  val selStms = Copy.copy {src=rev ss, dst=params}
      in
	  print ((Int.toString (length params))^"\n");
	  print ((Int.toString (length argRoots))^"\n");
	  print ((Int.toString (length selStms))^"\n");
	  {stms=List.concat [
	  (* allocate a heap object for GC roots *)
	  [move (rootReg, regExp apReg)],
	  regStms,
	  allocStms,
	  (* save the root pointer in the argReg *)
	  [move (argReg, regExp rootReg)],
	  (* perform the GC *)
	  genGCCall (),
	  selStms,
	  [T.JMP (T.LABEL gcLbl, [gcLbl])]
	  ], liveOut=map (T.GPR o regExp o regGP) params}
      end (* genHeapCheck *)

  fun genFuncEntry varDefTbl (lab, convention) =
      let fun doBind (M.StdFunc {clos, arg, ret, exh}) = 
	      ([clos, arg, ret, exh], stdCallRegs)
	    | doBind (M.StdCont {clos, arg}) = ([clos, arg], stdContRegs)
	    | doBind (M.KnownFunc vs | M.Block vs) = (vs, [])
	  val (lhs, stdRegs) = doBind convention
	  val bindToRExp = bindToRExp varDefTbl
	  fun bindToLHS rs = ListPair.app bindToRExp (lhs, rs)		  
	  val {stms, regs} = 
	      (case stdRegs
		of [] => (* specialized calling convention or block *)
		   {stms=[], regs=map (gpReg o newReg) lhs}
		 | _ => (* standard calling convention *)
		   Copy.fresh (map gpReg stdRegs)
	      (* esac *))
      in 	
	  LabelCode.setParamRegs (lab, regs);
	  bindToLHS (map (regExp o regGP) regs);
	  stms
      end (* genFuncEntry *)

  val aTy = M.T_Any
  val retTy = M.T_Code [aTy]
  val kTy = M.T_OpenTuple [retTy]
  val intTy = M.T_Raw RawTypes.T_Int

  fun genModuleEntry code =
      let val entryLbl = 
	      (case code
		of M.FUNC {lab, ...} :: _ => LabelCode.getName lab
		 | _ => raise Fail ""
	      (* esac *))
	  val initLbl = "initLbl"

	  val rReg = newReg ()

	  val {stms=argInitStms, rhs} = Alloc.genWrap (intTy, mltGPR argReg)

	  val retKStms = 
	      Alloc.genAlloc [(retTy, mkExp (T.LABEL (Label.global "returnloc")))]
	  val initKStms = Alloc.genAlloc 
		[(retTy, mkExp (T.LABEL (Label.global initLbl))),
		 (aTy, mltGPR closReg), 
		 (intTy, mltGPR argReg), (* FIXME: need a wrapped arg *)
		 (kTy, mltGPR rReg), 
		 (kTy, mltGPR exhReg)]
	  val mty = M.T_Tuple [retTy, aTy, intTy, kTy, kTy]
	  val baseReg = newReg ()
	  val baseReg' = newReg ()
	  val kReg = newReg ()
	  val aReg = newReg ()
	  fun doSelect (r, i) = move (r, 
	      Alloc.select {lhsTy=ty, mty=mty, i=i, base=regExp aReg} )
	  val (selects, _) = 
	      foldl (fn (r, (ss, i)) => (doSelect (r, i) :: ss, i+1)) ([], 1) stdCallRegs
      in
	  {modEntryLbl="mantentry",
	   entryStms=List.concat [
	   (* wrap the integer argument *)
	   argInitStms,
	   [move (argReg, regExp rhs)], 
	   (* allocate and save the outer continuation *)
	   [move (rReg, regExp apReg)],
	   retKStms,
	   (* allocate and save the initial continuation *)
	   [move (retReg, regExp apReg)],
	   initKStms,
	   (* jump to the module initialization function *)
	   genJump (T.LABEL entryLbl, [], [], [])
	   ],
	   initLbl=initLbl,
	   initStms=List.concat [
	   [move (baseReg, regExp argReg)],
	   [move (kReg, Alloc.select {lhsTy=ty, mty=mty, i=1, base=regExp baseReg})],

	   [move (aReg, regExp closReg)],
	   selects,
	   genJump (regExp kReg, [],  [], [])
	   ]
	  }
      end (* genModuleEntry *)

end (* HeapTransferFn *)
