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
  val stdCallRegs = [closReg, argReg, retReg, exhReg]
  val stdContRegs = [closReg, argReg]

  fun fail s = raise Fail s

  fun newLabel s = Label.label s () 
  fun labelToMLRisc l = newLabel (M.Label.toString l)
  fun newReg _ = Cells.newReg ()
  fun genLit (Literal.Int i) = MTy.EXP (ty, T.LI i)
    | genLit (Literal.Bool true) = MTy.EXP (ty, T.LI Spec.trueRep)
    | genLit (Literal.Bool false) = MTy.EXP (ty, T.LI Spec.falseRep)
    | genLit (Literal.Float f) = fail "todo"
(*    | genLit (Literal.Char c) = fail "todo"*)
    | genLit (Literal.String s) = fail "todo"
  fun intLit i = T.LI (T.I.fromInt (ty, i))
  fun gpReg r = MTy.GPReg (ty, r)
  fun regGP (MTy.GPReg (_, r)) = r
    | regGP (MTy.FPReg (_, r)) = r
  fun gprToMLTree r = MTy.GPR (ty, r)
  fun regExp r = T.REG (ty, r)
  fun mkExp e = MTy.EXP (ty, e)
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
	  fun bindToRExp' (ty, x, e) = setDefOf (x, MTy.EXP (ty, e))
	  fun bindToRExp (x, e) = bindToRExp' (BE.Types.szOf (Var.typeOf x), x, e)
	  fun bindToReg (x, r) = 
	      let val ty = BE.Types.szOf (Var.typeOf x)
	      in 
		  bindToRExp' (ty, x, T.REG (ty, r))
	      end

	  val genPrim = #gen (Prim.genPrim {varDefTbl=varDefTbl})

	  fun genStdTransfer (tgt, labels, args, stdRegs) =
	      let val vs = tgt :: args
		  val allRs as tgtReg :: rs = map newReg vs
		  val stdRegs = map gpReg stdRegs
	      in
		  (* copy the function and its args into temp registers *)
		  emitStms (BE.Copy.copy {src=map getDefOf vs, dst=map gpReg allRs});
		  (* jump to the function with its fresh args *)
		  genJump (regExp tgtReg, labels, stdRegs, map gprToMLTree rs);
		  map (T.GPR o regExp o regGP) stdRegs
	      end (* genStdTransfer *)

	  and genTransfer (t as M.StdApply {f, clos, arg, ret, exh}) =
	      exitBlock (genStdTransfer (f, [] (*CFACFG.labelsOf t*), 
					 [clos, arg, ret, exh], stdCallRegs))
	    | genTransfer (t as M.StdThrow {k, clos, arg}) =
	      exitBlock (genStdTransfer (k, [] (*CFACFG.labelsOf t*), 
					 [clos, arg], stdContRegs))
	    | genTransfer (M.Apply {f, args}) = fail "todo"
	    | genTransfer (M.Goto jmp) = genGoto jmp
	    | genTransfer (M.If (c, jT as (lT, argsT), jF)) = 
	      let val labT = newLabel "L_true"
	      in 
		  emit (T.BCC (cdefOf c, labT));
		  genGoto jF;
		  defineLabel labT;
		  genGoto jT
	      end
	    | genTransfer (M.Switch (c, js, jOpt)) = fail "todo"
	    (* invariant: #2 gc = #2 nogc (their arguments are the same) *)
	    | genTransfer (M.HeapCheck {szb, gc=(_, argRoots), nogc=(nogc, _)}) = 
	      let val nogcLbl = BE.LabelCode.getName nogc
		  val allocCheck = BE.Alloc.genAllocCheck szb
		  val rootReg = newReg ()
		  fun argInfo (a, (freshRegs, mtys, mlrTrees)) = 
		      let val mty = Var.typeOf a
			  val e = getDefOf a
		      in 
			  (newReg a :: freshRegs, mty :: mtys, e :: mlrTrees)
		      end (* argInfo *)
		  val (freshRegs, mtys, mlrTrees) = foldl argInfo ([],[],[]) argRoots
		  val regStms = BE.Copy.copy {src=mlrTrees, dst=map gpReg freshRegs}
		  (* allocate space on the heap for the roots *)
		  val allocStms = BE.Alloc.genAlloc (ListPair.zip (mtys, 
							map gprToMLTree freshRegs))
		  val noGCParams = BE.LabelCode.getParamRegs nogc
	      in 
		  (* allocate a heap object for GC roots *)
		  emit (move (rootReg, regExp apReg));
		  emitStms regStms;
		  emitStms allocStms;
		  (* save the root pointer in the argReg *)
		  emit (move (argReg, regExp rootReg));
		  (* set up the return pointer*)
		  emit (move (retReg, T.LABEL nogcLbl));
		  emitStms (BE.Transfer.genGCCall ())
	      end

	  and genGoto (l, args) =
	      let val name = BE.LabelCode.getName l
		  val params = BE.LabelCode.getParamRegs l
	      in
		  genJump (T.LABEL name, [name], params, map getDefOf args)
	      end (* genGoto *)

	  and genJump (target, ls, params, args) =
	      let val stms = BE.Copy.copy {src=args, dst=params}
	      in
		  emitStms stms;
		  emit (T.JMP (target, ls))
	      end (* genJump *)
							  
	  and genExp e = 
	      let val (mlrEs, mlrStms) = genExp' e
		  val lhs = M.lhsOfExp e
		  val regs = map newReg lhs
		  val stms = BE.Copy.copy {src=mlrEs, dst=map gpReg regs}
	      in
		  ListPair.app bindToReg (lhs, regs);
		  emitStms (stms @ mlrStms)
	      end

	  and genExp' (M.E_Var (_, rhs)) = (map getDefOf rhs, [])
	    | genExp' (M.E_Literal (_, lit)) = ([genLit lit], [])
	    | genExp' (M.E_Label (_, l)) = 
	      ([MTy.EXP (ty, T.LABEL (BE.LabelCode.getName l))], [])
	    | genExp' (M.E_Select (lhs, i, v)) =
	      ([select (BE.Types.szOf (Var.typeOf lhs), 
			Var.typeOf v, i, defOf v)], [])
	    | genExp' (M.E_Alloc (_, vs)) = 
	      ([gprToMLTree apReg], 
	       genAlloc (map (fn v => (Var.typeOf v, getDefOf v)) vs))
	    | genExp' (M.E_Wrap (_, v)) = fail "todo"
	    | genExp' (M.E_Unwrap (_, v)) = fail "todo"
	    | genExp' (M.E_Prim (_, p)) = ([genPrim p], [])
	    | genExp' (M.E_CCall (_, f, args)) = fail "todo"

	  (* bind the parameters for the body of a FUNC *)
	  fun bindParams (lab, conv) =
	      let fun doBind (M.StdFunc {clos, arg, ret, exh}) = 
		      ([clos, arg, ret, exh], stdCallRegs)
		    | doBind (M.StdCont {clos, arg}) = ([clos, arg], stdContRegs)
		    | doBind (M.KnownFunc vs | M.Block vs) = (vs, [])
		  val (lhs, stdRegs) = doBind conv
		  fun bindToLHS rs = ListPair.app bindToRExp (lhs, rs)		  
		  val mkReg = regExp o regGP
	      in 			  
		  (case stdRegs
		    of [] => (* specialized calling convention or block *)
		       let val regs = map (gpReg o newReg) lhs
		       in
			   BE.LabelCode.setParamRegs (lab, regs);
			   bindToLHS (map mkReg regs)
		       end		       
		     | _ =>  (* standard calling convention *)
		       let val {stms, regs} = BE.Copy.fresh (map gpReg stdRegs)
		       in 
			   BE.LabelCode.setParamRegs (lab, regs);
			   bindToLHS (map mkReg regs);
			   emitStms stms
		       end
		  (* esac *))
	      end

	  fun genFunc (M.FUNC {lab, entry, body, exit}) =
	      let val funcAnRef = getAnnotations ()
		  val frame = BE.SpillLoc.getFuncFrame lab
		  fun emitLabel () = 
		      (case M.Label.kindOf lab
			of M.Export s => ( 
			   defineLabel (BE.LabelCode.getName lab);
			   defineLabel (Label.global s) )
			 | M.Local => defineLabel (BE.LabelCode.getName lab)
			 | M.Extern _ => fail "attempt to define a C function"
		      (* esac *))
		  fun emitGlobal (M.Export s) = pseudoOp (P.global (Label.global s))
		    | emitGlobal _ = ()
	      in
		  beginCluster 0;		  
		  emitGlobal (M.Label.kindOf lab);
		  pseudoOp P.text;		  
		  emitLabel ();
		  funcAnRef := (#create BE.SpillLoc.frameAn) frame :: (!funcAnRef);
		  bindParams (lab, entry);
		  app genExp body;
		  genTransfer exit;
		  endCluster []; ()
	      end (* genFunc *)
      in
	  Cells.reset ();
	  app genFunc code
      end (* codeGen *)

end (* CodeGen *)
