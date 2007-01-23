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
  val stdArgReg = MTy.GPReg (ty, BE.Regs.stdArgReg)
  val retReg = stdArgReg (* same as stdArg in CPS *)
  val spReg = MTy.GPReg (ty, BE.Regs.spReg)

  fun fail s = raise Fail s

  fun newLabel s = Label.label s () 
  fun genLit (Literal.Int i) = T.LI i
    | genLit (Literal.Bool true) = T.LI Spec.trueRep
    | genLit (Literal.Bool false) = T.LI Spec.falseRep
    | genLit (Literal.Float f) = fail "todo"
    | genLit (Literal.Char c) = fail "todo"
    | genLit (Literal.String s) = fail "todo"
  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun reg c = MTy.GPR (ty, c)
  fun mkExp e = MTy.EXP (ty, e)

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
	  fun bindToRExp (x, rhs) = setDefOf (x, mkExp rhs)

	  fun genExp (M.Exp (_, e)) = genExp' e

	  and genExp' (M.E_Let (lhs, rhs, e)) =  (
	      genRHS (lhs, rhs);
	      genExp e )
	    | genExp' (M.E_Goto (l, args)) = genJump (l, args)
	    | genExp' (M.E_If (tst, jT as (lT, argsT), jF)) = 
	      let val labT = newLabel "L_true"
	      in 
		  emit (T.BCC (cdefOf tst, labT));
		  genJump jF;
		  defineLabel labT;
		  genJump jT
	      end
	    | genExp' (M.E_Switch (tst, jumps, jDefault)) = fail "todo"
	    | genExp' (M.E_HeapCheck (i, e)) = fail "todo"
	    | genExp' (M.E_Apply (f, args)) = fail "todo"	      
	    | genExp' (M.E_Throw (k, args)) = fail "todo"

	  and genJump (l, args) =
	      let val name = LabelCode.getName l
		  val params = LabelCode.getParamRegs l
	      in
		  genJump' (T.LABEL name, [name], params, args)
	      end
	  and genJump' (target, ls, params, args) =
	      let val args = map getDefOf args
		  val stms = Copy.copy {src=args, dst=params}
	      in
		  emitStms stms;
		  emit (T.JMP (target, ls))
	      end

	  and genRHS (_, M.E_Prim p) = fail "todo"
	    | genRHS ([x], rhs) = bindToRExp (x, 
	      (case rhs
		of M.E_Var v => defOf v
		 | M.E_Literal l => genLit l
		 | M.E_Label l => T.LABEL (LabelCode.getName l)
		 | M.E_Select (i, v) =>
		   let val r = T.ADD (ty, litFromInt i, defOf v)
		   in 
		       T.LOAD (ty, r, memory)
		   end
		 | M.E_Alloc (ty, vs) => fail "todo"
		 | M.E_CCall (f, args) => fail "todo"
		 | M.E_Prim _ => fail "impossible"
	      (* esac *)))
	    | genRHS _ = fail ""


	  fun genFunc (M.FUNC {lab, kind, params, body}) =
	      let val regs = LabelCode.getParamRegs lab
	      in 
		  beginCluster 0;
		  pseudoOp P.text;
		  ListPair.app (fn (r,x) => setDefOf (x, MTy.regToTree r)) 
			       (LabelCode.getParamRegs lab, params);
		  defineLabel (LabelCode.getName lab);
		  genExp body;
		  endCluster []; ()
	      end (* genFunc *)

	  fun genFuncs () = app genFunc code
      in
	  Cells.reset ();
	  AsmStream.withStream dst genFuncs ()
      end (* codeGen *)

end (* CodeGen *)
