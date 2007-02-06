(* code-gen-fn.sml
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Translate the CFG representation into MLRISC trees.
 *)

functor CodeGenFn (BE : BACK_END) :> CODE_GEN = struct

  structure Spec = BE.Spec
  structure Stream = BE.MLTreeComp.TS.S
  structure P = BE.ManticorePseudoOps
  structure Instr = BE.MLTreeComp.I
  structure Cells = Instr.C
  structure T = BE.MLTreeComp.TS.T
  structure M = CFG
  structure Ty = CFGTy
  structure Var = M.Var
  structure MTy = BE.MTy
  structure Prim = PrimGenFn (structure BE = BE)

  val wordSzB = Word.toInt Spec.wordSzB
  val ty = wordSzB * 8
  val memory = ManticoreRegion.memory
  val argReg = BE.Regs.argReg
  val retReg= BE.Regs.retReg
  val closReg = BE.Regs.closReg
  val exhReg = BE.Regs.exhReg
  val apReg = BE.Regs.apReg
  val stdCallRegs = BE.Transfer.stdCallRegs
  val stdContRegs = BE.Transfer.stdContRegs

  fun fail s = raise Fail s
  fun newLabel s = Label.label s () 
  fun labelToMLRisc l = newLabel (M.Label.toString l)
  fun newReg _ = Cells.newReg ()
  fun mkExp e = MTy.EXP (ty, e)
  fun genLit (Literal.Int i) = MTy.EXP (ty, T.LI i)
    | genLit (Literal.Bool true) = MTy.EXP (ty, T.LI Spec.trueRep)
    | genLit (Literal.Bool false) = MTy.EXP (ty, T.LI Spec.falseRep)
    | genLit (Literal.Float f) = fail "todo"
    | genLit (Literal.Char c) = fail "todo"
    | genLit (Literal.String s) = fail "todo"
  fun intLit i = T.LI (T.I.fromInt (ty, i))
  fun gpReg r = MTy.GPReg (ty, r)
  fun regGP (MTy.GPReg (_, r)) = r
    | regGP (MTy.FPReg (_, r)) = r
  fun mltGPR r = MTy.GPR (ty, r)
  fun regExp r = T.REG (ty, r)
  fun move (r, e) = T.MV (ty, r, e)

  fun select (lhsTy, mty, i, e) =
      let fun offsetOf (M.T_Tuple tys) = BE.Alloc.offsetOf {tys=tys, i=i}
	    | offsetOf _ = fail "offsetOf: non-tuple type"
	  val offset = offsetOf mty
      in 
	  MTy.EXP (lhsTy, T.LOAD (lhsTy, T.ADD (ty, e, intLit offset), memory))
      end (* select *)

  val genAlloc = BE.Alloc.genAlloc

  fun codeGen {dst, code=M.MODULE {code, funcs}} = 
      let val mlStrm = BE.MLTreeComp.selectInstructions (BE.CFGGen.build ())
	  (* extract operations from the emitter's streams *)
	  val Stream.STREAM { 
	      beginCluster, getAnnotations, comment, emit, defineLabel, 
	      entryLabel, exitBlock, pseudoOp, endCluster, ...} = mlStrm
	  val endCluster = BE.compileCFG o endCluster
	  val emitStms = app emit

	  val varDefTbl = BE.VarDef.newTbl ()
	  val getDefOf = BE.VarDef.getDefOf varDefTbl
	  val setDefOf = BE.VarDef.setDefOf varDefTbl
	  val defOf = BE.VarDef.defOf varDefTbl
	  val fdefOf = BE.VarDef.fdefOf varDefTbl
	  val cdefOf = BE.VarDef.cdefOf varDefTbl
	  val genStdTransfer = BE.Transfer.genStdTransfer varDefTbl
	  val genGoto = BE.Transfer.genGoto varDefTbl
	  fun bindToRExp' (ty, x, e) = setDefOf (x, MTy.EXP (ty, e))
	  fun bindToReg (x, r) = 
	      bindToRExp' (BE.Types.szOf (Var.typeOf x), x, T.REG (ty, r))

	  val genPrim = #gen (Prim.genPrim {varDefTbl=varDefTbl})

	  fun genTransfer (t as M.StdApply {f, clos, arg, ret, exh}) =
	      let val {stms, liveOut} = 
		      genStdTransfer (f, [] (*CFACFG.labelsOf t*), 
				      [clos, arg, ret, exh], stdCallRegs)
	      in
		  emitStms stms;
		  exitBlock liveOut
	      end
	    | genTransfer (t as M.StdThrow {k, clos, arg}) =
	      let val {stms, liveOut} = 
		      (genStdTransfer (k, [] (*CFACFG.labelsOf t*), 
				       [clos, arg], stdContRegs))
	      in
		  emitStms stms;
		  exitBlock liveOut
	      end
	    | genTransfer (M.Apply {f, args}) = fail "todo"
	    | genTransfer (M.Goto jmp) = emitStms (genGoto jmp)
	    | genTransfer (M.If (c, jT as (lT, argsT), jF)) = 
	      let val labT = newLabel "L_true"
	      in 
		  emit (T.BCC (cdefOf c, labT));
		  emitStms (genGoto jF);
		  defineLabel labT;
		  emitStms (genGoto jT)
	      end
	    | genTransfer (M.Switch (c, js, jOpt)) = fail "todo"
	    (* invariant: #2 gc = #2 nogc (their arguments are the same) *)
	    | genTransfer (M.HeapCheck hc) = 
	      emitStms (BE.Transfer.genHeapCheck varDefTbl hc)
							  
	  and genExp e = 
	      let val {rhsEs, stms} = genExp' e
		  val lhs = M.lhsOfExp e
		  val regs = map newReg lhs
		  val copyStms = BE.Copy.copy {src=rhsEs, dst=map gpReg regs}
	      in
		  ListPair.app bindToReg (lhs, regs);
		  emitStms (copyStms @ stms)
	      end

	  and genExp' (M.E_Var (_, rhs)) = {rhsEs=map getDefOf rhs, stms=[]}
	    | genExp' (M.E_Literal (_, lit)) = {rhsEs=[genLit lit], stms=[]}						
	    | genExp' (M.E_Label (_, l)) = 
	      {rhsEs=[mkExp (T.LABEL (BE.LabelCode.getName l))], stms=[]}
	    | genExp' (M.E_Select (lhs, i, v)) =
	      {rhsEs=[select (BE.Types.szOf (Var.typeOf lhs), 
			      Var.typeOf v, i, defOf v)], stms=[]}
	    | genExp' (M.E_Alloc (_, vs)) = 
	      {rhsEs=[mltGPR apReg], 
	       stms=genAlloc (map (fn v => (Var.typeOf v, getDefOf v)) vs)}
	    | genExp' (M.E_Wrap (_, v)) = fail "todo"
	    | genExp' (M.E_Unwrap (_, v)) = fail "todo"
	    | genExp' (M.E_Prim (_, p)) = {rhsEs=[genPrim p], stms=[]}
	    | genExp' (M.E_CCall (_, f, args)) = fail "todo"


	  fun genFunc (M.FUNC {lab, entry, body, exit}) =
	      let val funcAnRef = getAnnotations ()
		  val frame = BE.SpillLoc.getFuncFrame lab
		  fun emitLabel () = 
		      (case M.Label.kindOf lab
			of M.Export s => ( 
			   pseudoOp (P.global (Label.global s));
			   defineLabel (BE.LabelCode.getName lab);
			   defineLabel (Label.global s) )
			 | M.Local => defineLabel (BE.LabelCode.getName lab)
			 | M.Extern _ => fail "attempt to define a C function"
		      (* esac *))		  
	      in
		  beginCluster 0;		  
		  pseudoOp P.text;		  
		  emitLabel ();
		  funcAnRef := (#create BE.SpillLoc.frameAn) frame :: (!funcAnRef);
		  emitStms (BE.Transfer.genLabelEntry varDefTbl (lab, entry));
		  app genExp body;
		  genTransfer exit;
		  endCluster []; ()
	      end (* genFunc *)
      in
	  Cells.reset ();
	  app genFunc code
      end (* codeGen *)

end (* CodeGen *)
