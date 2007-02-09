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
  fun fail s = raise Fail s
  val toGPR = T.GPR o regExp o regGP

  fun bindToRExp' varDefTbl (ty, x, e) = 
      VarDef.setDefOf varDefTbl (x, MTy.EXP (ty, e))
  fun bindToRExp varDefTbl (x, e) = 
      bindToRExp' varDefTbl (Types.szOf (Var.typeOf x), x, e)

  fun select' (lhsTy, mty, i, e) =
      let fun offsetOf (M.T_Tuple tys) = Alloc.offsetOf {tys=tys, i=i}
	    | offsetOf (M.T_Code tys) = Alloc.offsetOf {tys=tys, i=i}
	    | offsetOf _ = fail "offsetOf: non-tuple type"
	  val offset = offsetOf mty
      in 
	  T.LOAD (lhsTy, T.ADD (ty, e, intLit offset), memory)
      end (* select' *)
  fun select (lhsTy, mty, i, e) = MTy.EXP (lhsTy, select' (lhsTy, mty, i, e))

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

  fun genStdTransfer varDefTbl (tgt, tgtReg, args, argRegs, stdRegs) =
      let val getDefOf = VarDef.getDefOf varDefTbl
	  val stdRegs = map gpReg stdRegs 
      in
	  {stms=List.concat [
	   (* copy the function and its args into temp registers *)
	   Copy.copy {src=map getDefOf (tgt :: args), 
		      dst=map gpReg (tgtReg :: argRegs)},
	   (* jump to the function with its fresh args *)
	   genJump (regExp tgtReg, [] (* FIXME *), stdRegs, map mltGPR argRegs)],
	   liveOut=map toGPR stdRegs}
      end (* genStdTransfer *)

  fun genStdCall varDefTbl {f, clos, arg, ret, exh} = 
      let val getDefOf = VarDef.defOf varDefTbl
	  val args = [clos, arg, ret, exh]
	  val argRegs = map newReg args
	  val {stms, liveOut} =
	      genStdTransfer varDefTbl (f, newReg f, args, argRegs, stdCallRegs)
      in
	  {stms=stms, liveOut=liveOut}
      end (* genStdCall *)

  fun genStdThrow varDefTbl {k, clos, arg} = 
      let val getDefOf = VarDef.defOf varDefTbl
	  val kReg = newReg k
	  val argRegs = map newReg [clos, arg]
	  val {stms, liveOut} = 
	      genStdTransfer varDefTbl (k, kReg, [clos, arg], argRegs, stdContRegs)
      in 
	  {stms=List.concat [
	   [move (kReg, select' (ty, M.Var.typeOf k, 0, getDefOf k))],
	   stms
	   ],
	   liveOut=liveOut}
      end (* genStdThrow *)
      
  fun genHeapCheck varDefTbl {szb, gc=(gcLbl, argRoots), nogc} =
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
	  List.concat [
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
	  ]
      end (* genHeapCheck *)

  fun genFuncEntry varDefTbl (lab, convention) =
      let val bindToRExp = bindToRExp varDefTbl	  
	  fun doBind (M.StdFunc {clos, arg, ret, exh}) = 
	      ([clos, arg, ret, exh], stdCallRegs)
	    | doBind (M.StdCont {clos, arg}) = ([clos, arg], stdContRegs)
	    | doBind (M.KnownFunc vs | M.Block vs) = (vs, [])
	  val (lhs, stdRegs) = doBind convention
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

  fun genModuleEntry code =
      let val entryLbl = 
	      (case code
		of M.FUNC {lab, ...} :: _ => LabelCode.getName lab
		 | _ => raise Fail ""
	      (* esac *))
      (* expect the return pointer in the argument reg *)
	  val allocStms = Alloc.genAlloc [(M.T_Code [M.T_Any], mltGPR argReg)]
      in
	  List.concat [
	  [move (retReg, regExp apReg)],
	  allocStms,
	  genJump (T.LABEL entryLbl, [], [], [])
	  ]
      end (* genModuleEntry *)

end (* HeapTransferFn *)
