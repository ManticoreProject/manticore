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

  (* Bogus binding to force inclusion of MLRisc controls *)
  val debug : bool Controls.control = CodegenControls.debug

  val ty = MTy.wordTy

  fun fail s = raise Fail s
  fun newLabel s = Label.label s () 
  fun labelToMLRisc l = newLabel (M.Label.toString l)
  fun newReg _ = Cells.newReg ()
  fun newFReg _ = Cells.newFreg ()
  fun mkExp e = MTy.EXP (ty, e)
  fun wordLit i = T.LI(T.I.fromInt (ty, Word.toIntX i))
  fun gpReg r = MTy.GPReg (ty, r)
  fun regGP (MTy.GPReg (_, r)) = r
    | regGP (MTy.FPReg (_, r)) = r
  fun mltGPR r = MTy.GPR (ty, r)
  fun regExp r = T.REG (ty, r)
  fun move (r, e) = T.MV (ty, r, e)
  fun freshMv e = let val r = newReg ()
	in
	  {reg=r, mv=move (r, e)}
	end (* freshMv *)
  fun note (stm, msg) =
        T.ANNOTATION(stm, #create MLRiscAnnotations.COMMENT msg)
  fun select (lhsTy, mty, i, e) = 
        BE.Alloc.select {lhsTy=lhsTy, mty=mty, i=i, base=e}
  fun addrOf (lhsTy, mty, i, e) = 
        BE.Alloc.addrOf {lhsTy=lhsTy, mty=mty, i=i, base=e}
  val szOf = BE.Types.szOf o Var.typeOf

  fun codeGen {dst, code=M.MODULE {name, externs, code}} = 
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
	  val bind = BE.VarDef.bind varDefTbl
	  fun flushLoads () = (case BE.VarDef.flushLoads varDefTbl
	      of [] => ()
	       | stms => (comment "flushLoads"; emitStms stms)
	      (* esac *))
	  val genGoto = BE.Transfer.genGoto varDefTbl
	  val genPrim = #gen (Prim.genPrim {varDefTbl=varDefTbl})

	  val floatTbl = FloatLit.new ()
	  fun emitFltLit ((sz, f), l) = (
	      pseudoOp P.alignData;
	      defineLabel l;
	      pseudoOp (P.float(sz, [f])))					
	  val strTbl = StringLit.new ()
	  fun emitStrLit (s, l) = (
	      defineLabel l;
	      pseudoOp (P.asciz s) )
	  (* use a special encoding for enums to distinguish them from pointers:
	   * enum(e) => 2*e+1
	   *)
	  fun encodeEnum e = Word.<<(e, 0w1) + 0w1
	  fun genLit (ty, Literal.Enum c) = 
		MTy.EXP(ty, T.LI(T.I.fromWord (ty, encodeEnum c)))
	    | genLit (ty, Literal.StateVal n) =
	      (* we want the two low bits of the state-value representation to be zero *)
		MTy.EXP(ty, T.LI(T.I.fromWord (ty, Word.<<(n, 0w2))))
	    | genLit (ty, Literal.Int i) = MTy.EXP(ty, T.LI i)
	    | genLit (fty, Literal.Float f) = let
		val lbl = FloatLit.addLit (floatTbl, (fty, f))
		in
		  MTy.FEXP (fty, T.FLOAD (fty, T.LABEL lbl, ()))
		end
	    | genLit (_, Literal.Char c) = fail "todo"
	    | genLit (_, Literal.String s) = let
		val lbl = StringLit.addLit (strTbl, s)
		in
		  MTy.EXP (ty, T.LABEL lbl)
		end

	  fun genStdTransfer {stms, liveOut} = (
	      emitStms stms;
	      exitBlock liveOut )

	  (* Generate code for a control transfer, e.g., a function or
	   * a continuation call or a heap-limit check. *)
	  fun genTransfer (M.StdApply args) =
	      genStdTransfer (BE.Transfer.genStdApply varDefTbl args)
	    | genTransfer (M.StdThrow args) =
	      genStdTransfer (BE.Transfer.genStdThrow varDefTbl args)
	    | genTransfer (M.Apply args) = 
	      genStdTransfer (BE.Transfer.genApply varDefTbl args)
	    | genTransfer (M.Goto jmp) = emitStms (genGoto jmp)
	    | genTransfer (M.If (c, jT as (lT, argsT), jF)) = 
	      let val labT = newLabel "L_true"
	      in 
		  emit (T.BCC (cdefOf c, labT));
		  emitStms (genGoto jF);
		  defineLabel labT;
		  emitStms (genGoto jT)
	      end
	    | genTransfer (M.Switch (v, js, jOpt)) = let		  
		  (* put the switch value into reg *)
		  val {reg, mv} = freshMv (defOf v)
		  val _ = emit mv

		  (* compare the value with each branch *)
		  fun compareCase i = let
		      (* encode the case values according to the type of the switch value *)
		      val c = (case Var.typeOf v                      
				of CFGTy.T_Enum _ => 
				   (* enums use a special encoding, so we need to use it in cases *)
				   encodeEnum i
				 | _ => i
			      (* end case *))
		      in
		        T.CMP (ty, T.EQ, T.REG (ty, reg), wordLit c)
		      end (* compareCase *)

		  fun genTest ((i, (l, [])), exits) = 
		      (* if the jump target has no arguments, put it directly into the branch instruction *) 
		      (emit (T.BCC (compareCase i, BE.LabelCode.getName l));
		       exits)		      
		    | genTest ((i, jmp), exits) = let
                      val labT = newLabel "S_case"
		      val jmpStms = genGoto jmp
		      in		
			  emit (T.BCC (compareCase i, labT));
			  (labT, jmpStms) :: exits
		      end

		(* exit the code block if the value equals the case *)
		  val exits = foldl genTest [] js

		  fun emitJump (labT, jmpStms) = (
		      defineLabel labT;
		      emitStms jmpStms )
	      in		  
		  app emitJump (List.rev exits);
		  Option.app (fn defJmp => emitStms (genGoto defJmp)) jOpt
	      end
	    (* invariant: #2 gc = #2 nogc (their arguments are the same) *)
	    | genTransfer (M.HeapCheck hc) = 
	      let val {stms, retKLbl, retKStms, liveOut} = BE.Transfer.genHeapCheck varDefTbl hc
	      in 
		  (* emit code for the heap-limit test and the transfer into the GC *) 
		  emitStms stms;
		  emit (T.LIVE liveOut) ;
		  (* emit an entypoint and code for the return continuation  *)
		  entryLabel retKLbl;
		  emitStms retKStms  
	      end

	  (* Bind some CFG variables to MLRISC trees, possibly emitting 
	   * the tree if the variable has a useCount > 1. *)
	  and bindExp (lhs, rhs) = let
	      fun doBind (l, r) = emitStms (bind (l, r))	      
              in
                 ListPair.app doBind (lhs, rhs)
              end                 

	  (* Construct an MLRISC tree from a CFG expression. *)
	  and genExp frame = let
		fun gen (M.E_Var(lhs, rhs)) = 
		    ListPair.app setDefOf (lhs, map getDefOf rhs)
		  | gen (M.E_Const(lhs, lit)) = 
		    bindExp ([lhs], [genLit (szOf lhs, lit)])
		  | gen (M.E_Label(lhs, l)) = 
		    bindExp ([lhs], [mkExp (T.LABEL (BE.LabelCode.getName l))])
		  | gen (M.E_Select(lhs, i, v)) =  
		    bindExp ([lhs], [select (szOf lhs, Var.typeOf v, i, defOf v)])
		  | gen (M.E_Update(i, lhs, rhs)) = (
		    flushLoads ();
		    emit (T.STORE (szOf lhs,
				   T.ADD (ty, defOf lhs, T.LI (T.I.fromInt (ty, i))), 
					  defOf rhs, ManticoreRegion.memory)))
		  | gen (M.E_AddrOf(lhs, i, v)) = let
		      val addr = addrOf(szOf lhs,  Var.typeOf v, i, defOf v)
		      in
		        bindExp ([lhs], [MTy.EXP(ty, addr)])
		      end
		  | gen (M.E_Alloc (lhs, vs)) = 
		      let val {ptr, stms} = 
			      BE.Alloc.genAlloc (map (fn v => (Var.typeOf v, getDefOf v)) vs)
		      in 
			  emitStms stms;
			  bindExp ([lhs], [ptr])
		      end
		  | gen (M.E_Wrap (lhs, v)) = 
		      let val {ptr, stms} = BE.Alloc.genWrap (Var.typeOf v, getDefOf v)
		      in
			  emitStms stms;
			  bindExp ([lhs], [ptr])
		      end
		  | gen (M.E_Unwrap (lhs, v)) = 
		      bindExp ([lhs], [select (szOf lhs,
					       Var.typeOf v, 0, defOf v)])
		  | gen (M.E_Prim (lhs, p)) = emitStms (genPrim (lhs, p))
		  | gen (M.E_CCall (lhs, f, args)) = 
		      let val {stms, result} = 
			      BE.Transfer.genCCall varDefTbl 
				{frame=frame, lhs=lhs, f=f, args=args}
		      in
			  emitStms stms;
			  bindExp (lhs, result)
		      end
		  | gen (M.E_Cast(lhs, _, v)) = 
		      (* FIXME: should a cast affect anything here? *)
		      bindExp ([lhs], [getDefOf v])
		    (* vproc operations *)
		  | gen (M.E_HostVProc lhs) =
		      bindExp ([lhs], [BE.VProcOps.genHostVP])
		  | gen (M.E_VPLoad(lhs, offset, vproc)) =
		      bindExp ([lhs], [BE.VProcOps.genVPLoad varDefTbl (offset, vproc)])
		  | gen (M.E_VPStore(offset, vproc, v)) =
		      emitStms [BE.VProcOps.genVPStore varDefTbl (offset, vproc, v)]
		in
		  gen
		end (* genExp *)
	      
	  fun genFunc (M.FUNC {lab, entry, body, exit}) =
	      let fun emitLabel () = let
		      val label = BE.LabelCode.getName lab
		      in
		        (case M.Label.kindOf lab
			  of M.LK_Local {export=SOME s, ...} => ( 			   
			     pseudoOp (P.global (Label.global s));
			     entryLabel (Label.global s);
			     defineLabel label)
			   | M.LK_Local {func=CFG.FUNC{entry, ...}, ...} => 
			     (case entry
			       of CFG.Block _ => 
				  (* CFG.Blocks are only called within their own cluster *)
				  defineLabel label
				| _ => entryLabel label
			     (* end case *))
			   | _ => fail "emitLabel"
			(* end case *))
		      end (* emitLabel *)
		  val stms = BE.Transfer.genFuncEntry varDefTbl (lab, entry)
		  fun finish () = 
		      let val funcAnRef = getAnnotations ()
			  val frame = BE.SpillLoc.getFuncFrame lab
(* DEBUG *)
			  val _ = comment ("CFG function: "^CFG.Label.toString lab)
			  val regs = BE.LabelCode.getParamRegs lab
			  val regStrs = map (MTy.treeToString o MTy.regToTree) regs 
			  val regStrs = map (fn s => comment ("param:"^s^" ")) regStrs
(* DEBUG *)
		      in			  
			  funcAnRef := (#create BE.SpillLoc.frameAn) frame :: 
				       (!funcAnRef);
			  emitLabel ();
			  emitStms stms;
			  app (genExp frame) body;
			  genTransfer exit
		      end (* finish *)
	      in
		  finish
	      end (* genFunc *)

	  fun genCluster c =
	      let val finishers = map genFunc c
	      in 
 		  beginCluster 0;
		  pseudoOp P.text;
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
	      in
		  beginCluster 0;
		  pseudoOp P.text;
		  pseudoOp (P.global RuntimeLabels.entry);
		  entryLabel RuntimeLabels.entry;
		  finisher ();
		  endCluster []
	      end 
(* FIXME: alignment! *)
	  fun genLiterals () = (
	      beginCluster 0;
	      pseudoOp P.rodata;
	      (* runtime constant magic number for sanity test *)
	      pseudoOp (P.global RuntimeLabels.magic);
	      defineLabel RuntimeLabels.magic;
	      pseudoOp (P.int (P.I32, [Spec.ABI.magic]));
	      (* generate floats *)
	      FloatLit.appi emitFltLit floatTbl;
	      (* generate strings *)
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

   val codeGen : {code: CFG.module, dst: TextIO.outstream} -> unit =
      BasicControl.mkTracePassSimple
      {passName = "codeGen",
       pass = codeGen}

end (* CodeGen *)
