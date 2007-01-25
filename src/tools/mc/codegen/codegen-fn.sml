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
  structure Var = M.Var
  structure MTy = BE.MTy
  structure Copy = CopyFn (
                   structure MTy = MTy
		   structure Spec = Spec
		   structure Cells=Cells )
  structure VD = VarDefFn ( 
                   structure MTy = MTy
		   structure Spec = Spec
		   structure MLTreeComp = BE.MLTreeComp )
  structure LabelCode = BE.LabelCode

  val ty = (Word.toInt Spec.wordSzB) * 8
  val memory = ManticoreRegion.memory
  val argReg = BE.Regs.argReg
  val retReg= BE.Regs.retReg
  val closReg = BE.Regs.spReg
  val exhReg = BE.Regs.exhReg
  val stdCallRegs = [closReg, argReg, retReg, exhReg]
  val stdContRegs = [closReg, argReg]

  fun fail s = raise Fail s

  fun newLabel s = Label.label s () 
  fun labelToMLRisc l = newLabel (M.Label.toString l)
  fun newReg _ = Cells.newReg ()
  fun genLit (Literal.Int i) = T.LI i
    | genLit (Literal.Bool true) = T.LI Spec.trueRep
    | genLit (Literal.Bool false) = T.LI Spec.falseRep
    | genLit (Literal.Float f) = fail "todo"
    | genLit (Literal.Char c) = fail "todo"
    | genLit (Literal.String s) = fail "todo"
  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun gpReg r = MTy.GPReg (ty, r)
  fun gprToMLTree r = MTy.GPR (ty, r)
  fun regExp r = T.REG (ty, r)
  fun mkExp e = MTy.EXP (ty, e)
  fun load addr = T.LOAD (ty, addr, memory)
  fun move (r, e) = T.MV (ty, r, e)

  fun codeGen {dst, code=M.MODULE {code, funcs}} = 
      let val insnStrm = BE.CFGGen.build ()
	  val mlStrm = BE.MLTreeComp.selectInstructions insnStrm
	  (* extract operations from the emitter's streams *)
	  val Stream.STREAM { 
	      beginCluster, getAnnotations, comment, emit, defineLabel, 
	      entryLabel, exitBlock, pseudoOp, endCluster, ...} = mlStrm
	  val endCluster = BE.compileCFG o endCluster
	  val emitStms = app emit

	  val varDefTbl = VD.newTbl ()
	  val getDefOf = VD.getDefOf varDefTbl
	  val setDefOf = VD.setDefOf varDefTbl
	  val defOf = VD.defOf varDefTbl
	  val fdefOf = VD.fdefOf varDefTbl
	  val cdefOf = VD.cdefOf varDefTbl
	  fun bindToRExp (x, e) = setDefOf (x, mkExp e)

	  fun genStdTransfer (tgt, labels, args, stdRegs) =
	      let val vs = tgt :: args
		  val allRs as tgtReg :: rs = map newReg vs
		  val stdRegs = map gpReg stdRegs
	      in
		  emitStms (Copy.copy {src=map getDefOf vs, dst=map gpReg allRs});
		  genJump' (regExp tgtReg, labels, stdRegs, map gprToMLTree rs);
		  map (fn (MTy.GPReg (_, r)) => T.GPR (regExp r)) stdRegs
	      end (* genStdTransfer *)

	  and genTransfer (t as M.StdApply {f, clos, arg, ret, exh}) =
	      exitBlock (genStdTransfer (f, [] (*CFACFG.labelsOf t*), 
					 [clos, arg, ret, exh], stdCallRegs))
	    | genTransfer (t as M.StdThrow {k, clos, arg}) =
	      exitBlock (genStdTransfer (k, [] (*CFACFG.labelsOf t*), 
					 [clos, arg], stdContRegs))
	    | genTransfer (M.Apply {f, args}) = fail "todo"
	    | genTransfer (M.Goto jmp) = genJump jmp
	    | genTransfer (M.If (c, jT as (lT, argsT), jF)) = 
	      let val labT = newLabel "L_true"
	      in 
		  emit (T.BCC (cdefOf c, labT));
		  genJump jF;
		  defineLabel labT;
		  genJump jT
	      end
	    | genTransfer (M.Switch (c, js, jOpt)) = fail "todo"
	    | genTransfer (M.HeapCheck {szb, gc, nogc}) = fail "todo"

	  and genExp e = 
	      let val mlrEs = genExp' e
		  val lhs = M.lhsOfExp e
		  val regs = map newReg lhs
		  val stms = Copy.copy {src=map mkExp mlrEs, dst=map gpReg regs}
	      in
		  ListPair.app bindToRExp (lhs, map regExp regs);
		  emitStms stms
	      end

	  and genExp' (M.E_Var (_, rhs)) = map defOf rhs
	    | genExp' (M.E_Literal (_, lit)) = [genLit lit]
	    | genExp' (M.E_Label (_, l)) = [T.LABEL (LabelCode.getName l)]
	    | genExp' (M.E_Select (_, i, v)) =
	      let val offset = litFromInt (i * (Word.toInt Spec.wordSzB))
	      in
		  [load (T.ADD (ty, defOf v, offset))]
	      end
	    | genExp' (M.E_Alloc (_, vs)) = fail "todo"
	    | genExp' (M.E_Wrap (_, v)) = fail "todo"
	    | genExp' (M.E_Unwrap (_, v)) = fail "todo"
	    | genExp' (M.E_Prim (_, p)) = fail "todo"
	    | genExp' (M.E_CCall (_, f, args)) = fail "todo"

	  and genJump (l, args) =
	      let val name = LabelCode.getName l
		  val params = LabelCode.getParamRegs l
	      in
		  genJump' (T.LABEL name, [name], params, map getDefOf args)
	      end

	  and genJump' (target, ls, params, args) =
	      let val stms = Copy.copy {src=args, dst=params}
	      in
		  emitStms stms;
		  emit (T.JMP (target, ls))
	      end 

	  fun genFunc (M.FUNC {lab, entry, body, exit}) =
	      let fun doBind (M.StdFunc {clos, arg, ret, exh}) =
		      ([clos, arg, ret, exh], stdCallRegs, map newReg stdCallRegs)
		    | doBind (M.StdCont {clos, arg}) = 
		      ([clos, arg], stdContRegs, map newReg stdContRegs)
		    | doBind (M.KnownFunc vs | M.Block vs) = (vs, map newReg vs, [])
		  fun bindParams conv =
		      let val (lhs, rhs, temps) = doBind conv
			  val rhs' = map regExp rhs
			  val {stms, regs=ts} = Copy.fresh (map gpReg rhs)
			  val ts = map gpReg temps
			  fun bindToLHS rs = ListPair.app bindToRExp (lhs, rs)
		      in 			  
			  (case temps
			    of [] => (
			       LabelCode.setParamRegs (lab, map gpReg rhs);
			       bindToLHS rhs' )
			     | _ => (
			       LabelCode.setParamRegs (lab, ts);
			       bindToLHS (map regExp temps);
			       emitStms stms )
			  (* esac *))
		      end
		  (* get the spill-frame for this function *)
		  val funcAnRef = getAnnotations ()
		  val frame = BE.SpillLoc.getFuncFrame lab
	      in 
		  beginCluster 0;
		  pseudoOp P.text;
		  defineLabel (LabelCode.getName lab);
		  funcAnRef := ((#create BE.SpillLoc.frameAn) frame) :: (!funcAnRef);
		  bindParams entry;
		  app genExp body;
		  genTransfer exit;
		  endCluster []; ()
	      end (* genFunc *)

      in
	  Cells.reset ();
	  app genFunc code
      end (* codeGen *)

end (* CodeGen *)
