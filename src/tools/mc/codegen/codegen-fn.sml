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

  val ty = MTy.wordTy
  val memory = ManticoreRegion.memory
  val apReg = BE.Regs.apReg
  val stdCallRegs = BE.Transfer.stdCallRegs
  val genAlloc = BE.Alloc.genAlloc
  val iTy = BE.Types.szOf (Ty.T_Raw Ty.T_Int)

  fun fail s = raise Fail s
  fun newLabel s = Label.label s () 
  fun labelToMLRisc l = newLabel (M.Label.toString l)
  fun newReg _ = Cells.newReg ()
  fun mkExp e = MTy.EXP (ty, e)
  fun genLit (Literal.Int i) = MTy.EXP (iTy, T.LI i)
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
  fun freshMv e = 
      let val r = newReg ()
      in
	  {reg=r, mv=move (r, e)}
      end
  fun select (lhsTy, mty, i, e) = MTy.EXP (lhsTy, 
		BE.Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=e})

  fun codeGen {dst, code=M.MODULE {name, code}} = 
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
	      let val ty = BE.Types.szOf (Var.typeOf x)
	      in 
		  bindToRExp (ty, x, T.REG (ty, r))
	      end

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
	    | genTransfer (M.Switch (v, js, jOpt)) = 
	      let val {reg, mv} = freshMv (defOf v)
		  val _ = emit mv
		  fun compare i = T.CMP (ty, T.EQ, T.REG (ty, reg), intLit i)
		  fun genTest ((i, jmp), exits) =
		      let val labT = newLabel "S_true"
		      in			  
			  emit (T.BCC (compare i, labT));
			  (labT, genGoto jmp) :: exits
		      end
		  val exits = foldl genTest [] js
		  fun emitJ (labT, jmpStms) = (
		      defineLabel labT;
		      emitStms jmpStms )
	      in		  
		  Option.app (fn defJmp => emitStms (genGoto defJmp)) jOpt;
		  app emitJ (rev exits)
	      end
	    (* invariant: #2 gc = #2 nogc (their arguments are the same) *)
	    | genTransfer (M.HeapCheck hc) = 
	      let val {stms, liveOut} = BE.Transfer.genHeapCheck varDefTbl hc
	      in 
		  emitStms stms;
		  emit (T.LIVE liveOut)
	      end
							  
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
	      let val {ptr, stms} = 
		      genAlloc (map (fn v => (Var.typeOf v, getDefOf v)) vs)
	      in 
		  emitStms stms;
		  bindExp ([lhs], [ptr])
	      end
	    | genExp (M.E_Wrap (lhs, v)) = 
	      let val {ptr, stms} = BE.Alloc.genWrap (Var.typeOf v, getDefOf v)
	      in
		  emitStms stms;
		  bindExp ([lhs], [ptr])
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
			of M.LK_Local {export=SOME s, ...} => ( 
			   pseudoOp (P.global (Label.global s));
			   defineLabel (BE.LabelCode.getName lab);
			   defineLabel (Label.global s) )
			 | ( M.LK_None | M.LK_Local _ ) => (
			   comment (M.Label.toString lab);
			   defineLabel (BE.LabelCode.getName lab) )
			 | _ => fail "emitLabel"
		      (* esac *))
		  val stms = BE.Transfer.genFuncEntry varDefTbl (lab, entry)
		  fun finish () = 
		      let val _ = beginCluster 0
			  val funcAnRef = getAnnotations ()
			  val frame = BE.SpillLoc.getFuncFrame lab
		      in			  
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
	      let val {modEntryLbl, entryStms, initLbl, initStms} =
		      BE.Transfer.genModuleEntry code
		  val entryL = 
		      M.Label.newWithKind (Atom.atom modEntryLbl, 
					   CFG.LK_None, M.T_Any)
		  val _ = beginCluster 0
		  val funcAnRef = getAnnotations ()
		  val frame = BE.SpillLoc.getFuncFrame entryL
	      in		  
		  pseudoOp (P.global (Label.global modEntryLbl));
		  funcAnRef := (#create BE.SpillLoc.frameAn) frame :: 
			       (!funcAnRef);
		  pseudoOp P.text;		  
		  defineLabel (Label.global modEntryLbl);
		  emitStms entryStms;
		  defineLabel (Label.global initLbl);
		  emitStms initStms;
		  endCluster []; ()
	      end (* genModule *)

	  val finishers = map genFunc code
      in
	  Cells.reset ();	  
	  app (fn f => f()) finishers;
	  genModule () 
      end (* codeGen *)

end (* CodeGen *)
