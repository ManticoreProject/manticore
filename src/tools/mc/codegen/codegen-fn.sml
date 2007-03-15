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

  structure FloatLit = LiteralTblFn (
        type lit = (T.ty * FloatLit.float)
	val labelPrefix = "flt"
	fun hash (_, f) = FloatLit.hash f
	fun same ( (sz1 : T.ty, f1), (sz2, f2) ) =
	    (sz1 = sz2) andalso FloatLit.same(f1, f2) )

  structure StringLit = LiteralTblFn (
	type lit = string
	val labelPrefix = "str"
	val hash = HashString.hashString
	fun same (s1 : string , s2) = s1 = s2 )

  val ty = MTy.wordTy

  fun fail s = raise Fail s
  fun newLabel s = Label.label s () 
  fun labelToMLRisc l = newLabel (M.Label.toString l)
  fun newReg _ = Cells.newReg ()
  fun newFReg _ = Cells.newFreg ()
  fun mkExp e = MTy.EXP (ty, e)
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
      end (* freshMv *)
  fun note (stm, msg) =
      T.ANNOTATION(stm, #create MLRiscAnnotations.COMMENT msg)
  fun select (lhsTy, mty, i, e) = 
      BE.Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=e}

  fun codeGen {dst, code=M.MODULE {name, code}} = 
      let val mlStrm = BE.MLTreeComp.selectInstructions (BE.CFGGen.build ())
	  (* extract operations from the emitter's streams *)
	  val Stream.STREAM { 
	      beginCluster, getAnnotations, comment, emit, defineLabel, 
	      entryLabel, exitBlock, pseudoOp, endCluster, ...} = mlStrm
	  val emit = fn stm => emit (note (stm, BE.MLTreeUtils.stmToString stm))
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
	  fun bindToReg (x, r) = 
	      let val ty = BE.Types.szOf (Var.typeOf x)
	      in
		  BE.VarDef.bind varDefTbl (ty, x, T.REG (ty, r))
	      end (* bindToReg *)

	  val floatTbl = FloatLit.new ()
	  fun emitFltLit ((sz, f), l) = (
	      pseudoOp P.alignData;
	      defineLabel l;
	      pseudoOp (P.float(sz, [f])))					
	  val strTbl = StringLit.new ()
	  fun emitStrLit (s, l) = (
	      defineLabel l;
	      pseudoOp (P.asciz s) )
	  fun genLit (ty, Literal.Int i) = MTy.EXP (ty, T.LI i)
	    | genLit (ty, Literal.Bool true) = MTy.EXP (ty, T.LI Spec.trueRep)
	    | genLit (ty, Literal.Bool false) = MTy.EXP (ty, T.LI Spec.falseRep)
	    | genLit (fty, Literal.Float f) = 
	      let val lbl = FloatLit.addLit (floatTbl, (fty, f))
	      in
		  MTy.FEXP (fty, T.FLOAD (fty, T.LABEL lbl, ()))
	      end
	    | genLit (_, Literal.Char c) = fail "todo"
	    | genLit (_, Literal.String s) = 
	      let val lbl = StringLit.addLit (strTbl, s)
	      in
		  MTy.EXP (ty, T.LABEL lbl)
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
	      let (* put the value into reg *)
		  val {reg, mv} = freshMv (defOf v)
		  val _ = emit mv
		  (* compare the value with each branch *)
		  fun compare i = T.CMP (ty, T.EQ, T.REG (ty, reg), intLit i)
		  fun genTest ((i, jmp), exits) =
		      let val labT = newLabel "S_case"
			  val jmpStms = genGoto jmp
		      in		
			  emit (T.BCC (compare i, labT));
			  (labT, jmpStms) :: exits
		      end
		  (* exit the code block if the value equals the case *)
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
	    | genTransfer (M.Run args) =
		genStdTransfer (BE.Transfer.genRun varDefTbl args)
	    | getTransfer (M.Forward sign) =
		genStdTransfer (BE.Transfer.genForward varDefTbl sign)
							  
	  and bindExp (lhs, rhsEs) = 
	      let fun getReg (l, (rs, gprs)) = 
		      let val mty = Var.typeOf l
			  val ty = BE.Types.szOf mty
		      in
			  (case MTy.cfgTyToMLRisc mty
			    of MTy.K_FLOAT =>
			       let val r = newFReg ()
			       in
				   (MTy.FPR (ty, r) :: rs, MTy.FPReg (ty, r) :: gprs)
			       end
			     | MTy.K_INT => 
			       let val r = newReg ()
			       in
				   (MTy.GPR (ty, r) :: rs, MTy.GPReg (ty, r) :: gprs)
			       end
			     | _ => fail "getReg"
			  (* esac *))
		      end (* getReg *)
		  val (regs, gprs) = foldl getReg ([], []) lhs
		  val copyStms = BE.Copy.copy {src=rhsEs, dst=gprs}
	      in
		  ListPair.app setDefOf (lhs, regs);
		  emitStms copyStms
	      end (* bindExp *)

	  and genExp (M.E_Var (lhs, rhs)) = 
	      bindExp (lhs, map getDefOf rhs)
	    | genExp (M.E_Literal (lhs, lit)) = 
	      bindExp ([lhs], [genLit (BE.Types.szOf (Var.typeOf lhs), lit)])
	    | genExp (M.E_Label (lhs, l)) = 
	      bindExp ([lhs], [mkExp (T.LABEL (BE.LabelCode.getName l))])
	    | genExp (M.E_Select (lhs, i, v)) =  
	      bindExp ([lhs], [select (BE.Types.szOf (Var.typeOf lhs), 
				       Var.typeOf v, i, defOf v)])
	    | genExp (M.E_Alloc (lhs, vs)) = 
	      let val {ptr, stms} = 
		      BE.Alloc.genAlloc (map (fn v => (Var.typeOf v, getDefOf v)) vs)
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
	    | genExp (M.E_Prim (lhs, p)) = genPrim (lhs, p)
	    | genExp (M.E_CCall (lhs, f, args)) = 
	      let val {stms, result} = 
		      BE.Transfer.genCCall varDefTbl {lhs=lhs, f=f, args=args}
	      in
		  emitStms stms;
		  bindExp (lhs, result)
	      end
	    | genExp (M.E_Enum (lhs, c)) = 
	      bindExp ([lhs], [mkExp(T.LI (T.I.fromWord (ty, c)))])
	    | genExp (M.E_Cast (lhs, _, v)) = 
(* FIXME: should a cast affect anything here? *)
	      bindExp ([lhs], [getDefOf v])

	  fun genFunc (M.FUNC {lab, entry, body, exit}) =
	      let fun emitLabel () = 
		      (case M.Label.kindOf lab
			of M.LK_Local {export=SOME s, ...} => ( 
			   pseudoOp (P.global (Label.global s));
			   entryLabel (BE.LabelCode.getName lab);
			   entryLabel (Label.global s) )
			 | ( M.LK_None | M.LK_Local _ ) => (
			   comment (M.Label.toString lab);			   
			   defineLabel (BE.LabelCode.getName lab) )
			 | _ => fail "emitLabel"
		      (* esac *))
		  val stms = BE.Transfer.genFuncEntry varDefTbl (lab, entry)
		  fun finish () = 
		      let val funcAnRef = getAnnotations ()
			  val frame = BE.SpillLoc.getFuncFrame lab
(* DEBUG *)
			  val regs = BE.LabelCode.getParamRegs lab
			  val regStrs = map (MTy.treeToString o MTy.regToTree) regs 
			  val regStrs = map (fn s => comment ("param:"^s^" ")) regStrs
(* DEBUG *)
		      in			  
			  funcAnRef := (#create BE.SpillLoc.frameAn) frame :: 
				       (!funcAnRef);
			  pseudoOp P.text;		  
			  emitLabel ();
			  emitStms stms;
			  app genExp body;
			  genTransfer exit
		      end (* finish *)
	      in
		  finish
	      end (* genFunc *)

	  fun genCluster c =
	      let val finishers = map genFunc c
	      in 
 		  beginCluster 0;
		  app (fn f => f ()) finishers;
		  endCluster []
	      end (* genCluster *)

	  (* The first function of the module is its entry point.
	   * The code generator tacks on the module entry label,
	   * "mantEntry", to indicate where the RTS can execute the
	   * module.
	   *)
	  fun genModuleEntry f =
	      let val finisher = genFunc f
		  val entryL = Label.global "mantEntry"
	      in
		  beginCluster 0;
		  pseudoOp (P.global entryL);
		  entryLabel entryL;
		  finisher ();
		  endCluster []
	      end 

	  fun genLiterals () = (
	      beginCluster 0;
	      pseudoOp P.rodata;
	      FloatLit.appi emitFltLit floatTbl;
	      StringLit.appi emitStrLit strTbl;
	      endCluster []
	  )

	  val entryFunc :: code = code
	  val clusters = GenClusters.clusters code
      in
	  Cells.reset ();	  
	  app genCluster clusters;
	  genModuleEntry entryFunc;
	  genLiterals () 
      end (* codeGen *) 

end (* CodeGen *)
