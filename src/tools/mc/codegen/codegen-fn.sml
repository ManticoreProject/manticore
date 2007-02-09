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
  val apReg = BE.Regs.apReg
  val stdCallRegs = BE.Transfer.stdCallRegs

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
	    | offsetOf (M.T_Code tys) = BE.Alloc.offsetOf {tys=tys, i=i}
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
	  val genGoto = BE.Transfer.genGoto varDefTbl
	  val genPrim = #gen (Prim.genPrim {varDefTbl=varDefTbl})
	  fun bindToRExp (ty, x, e) = setDefOf (x, MTy.EXP (ty, e))
	  fun bindToReg (x, r) = 
	      bindToRExp (BE.Types.szOf (Var.typeOf x), x, T.REG (ty, r))

	  fun genStdTransfer {stms, liveOut} = (
	      emitStms stms;
	      exitBlock liveOut )

	  fun genTransfer (M.StdApply apply) =
	      genStdTransfer (BE.Transfer.genStdCall varDefTbl apply)
	    | genTransfer (M.StdThrow throw) =
	      genStdTransfer (BE.Transfer.genStdThrow varDefTbl throw)
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
							  
	  and bindExp (lhs, rhsEs) = 
	      let val regs = map newReg rhsEs
		  val copyStms = BE.Copy.copy {src=rhsEs, dst=map gpReg regs}
	      in
		  ListPair.app bindToReg (lhs, regs);
		  emitStms copyStms
	      end (* bindExp *)

	  and genExp (M.E_Var (lhs, rhs)) = 
	      bindExp (lhs, map getDefOf rhs)
	    | genExp (M.E_Literal (lhs, lit)) = bindExp ([lhs], [genLit lit])
	    | genExp (M.E_Label (lhs, l)) = 
	      bindExp ([lhs], [mkExp (T.LABEL (BE.LabelCode.getName l))])
	    | genExp (M.E_Select (lhs, i, v)) =
	      bindExp ([lhs], [select (BE.Types.szOf (Var.typeOf lhs), 
				       Var.typeOf v, i, defOf v)])
	    | genExp (M.E_Alloc (lhs, vs)) = 
	      let val stms = genAlloc (map (fn v => (Var.typeOf v, getDefOf v)) vs)
	      in 
		  bindExp ([lhs], [mltGPR apReg]);
		  emitStms stms
	      end
	    | genExp (M.E_Wrap (lhs, v)) = 
	      let val {rhsEs, stms} = BE.Alloc.genWrap (Var.typeOf v, getDefOf v)
	      in
		  bindExp ([lhs], rhsEs);
		  emitStms stms
	      end
	    | genExp (M.E_Unwrap (lhs, v)) = 
	      bindExp ([lhs], [select (BE.Types.szOf (Var.typeOf lhs), 
				       Var.typeOf v, 0, defOf v)])
	    | genExp (M.E_Prim (lhs, p)) = setDefOf (lhs, genPrim p)
	    | genExp (M.E_CCall (_, f, args)) = fail "todo"
	    | genExp (M.E_Enum (lhs, c)) = 
	      bindExp ([lhs], [mkExp(T.LI (T.I.fromWord (ty, c)))])
	    | genExp (M.E_Cast (lhs, _, v)) = 
	      bindExp ([lhs], [getDefOf v])

	  fun genFunc (M.FUNC {lab, entry, body, exit}) =
	      let fun emitLabel () = 
		      (case M.Label.kindOf lab
			of M.Export s => ( 
			   pseudoOp (P.global (Label.global s));
			   defineLabel (BE.LabelCode.getName lab);
			   defineLabel (Label.global s) )
			 | M.Local => defineLabel (BE.LabelCode.getName lab)
			 | M.Extern _ => fail "attempt to define a C function"
		      (* esac *))
		  val stms = BE.Transfer.genFuncEntry varDefTbl (lab, entry)
		  fun finish () = 
		      let val funcAnRef = getAnnotations ()
			  val frame = BE.SpillLoc.getFuncFrame lab
		      in
			  beginCluster 0;		  			 
			  funcAnRef := (#create BE.SpillLoc.frameAn) frame :: 
				       (!funcAnRef);
			  pseudoOp P.text;		  
			  emitLabel ();
			  emitStms stms;
			  app genExp body;
			  genTransfer exit;
			  endCluster []; ()  
		      end (* finish *)
	      in
		  finish
	      end (* genFunc *)

	  fun genModule () =
	      let val l = "mantentry"
		  val entryL = M.Label.new (Atom.atom l, 
					    CFG.Export l, M.T_Any)
		  val funcAnRef = getAnnotations ()
		  val frame = BE.SpillLoc.getFuncFrame entryL
	      in
		  beginCluster 0;		  
		  pseudoOp (P.global (Label.global l));
		  funcAnRef := (#create BE.SpillLoc.frameAn) frame :: 
			       (!funcAnRef);
		  pseudoOp P.text;		  
		  defineLabel (Label.global l);
		  emitStms (BE.Transfer.genModuleEntry code);
		  endCluster []; ()
	      end (* genModule *)

	  val finishers = map genFunc code
      in
	  Cells.reset ();
	  app (fn f => f()) finishers;
	  genModule () 
      end (* codeGen *)

end (* CodeGen *)
