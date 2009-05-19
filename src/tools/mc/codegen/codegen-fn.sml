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
  structure MChkTy = MLTreeCheckTy (
		        structure T = T
			val intTy = MTy.wordTy)

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

  structure TagLit = LiteralTblFn (
        type lit = string
	val labelPrefix = "tag"
	val hash = HashString.hashString
	fun same (s1 : string, s2) = s1 = s2 )

  (* Bogus binding to force inclusion of MLRisc controls *)
  val debug : bool Controls.control = CodegenControls.debug

  val v2s = Var.toString
  val i2s = Int.toString

  fun newLabel s = Label.label s () 
  fun newReg _ = Cells.newReg ()
  fun newFReg _ = Cells.newFreg ()

  val szOfVar = BE.Types.szOf o Var.typeOf

  val annotateInstrs = Controls.genControl {
	    name = "annotate-instrs",
	    pri = [5, 0],
	    obscurity = 1,
	    help = "annotate generated assembly code.",
	    default = false
	    }

  val _ = ControlRegistry.register CodegenControls.registry {
      ctl = Controls.stringControl ControlUtil.Cvt.bool annotateInstrs,
      envName = NONE}

  fun annotate (stm, msg) = if (msg <> "" andalso Controls.get annotateInstrs)
      then T.ANNOTATION(stm, #create MLRiscAnnotations.COMMENT msg)
      else stm

  fun annotateStms ([], msg) = []
    | annotateStms (stm :: stms, msg) = annotate(stm, "stdApply") :: stms

  fun codeGen {dst, code=M.MODULE {name, externs, code}} = let
      val _ =
         if BE.Spec.maxGPRArgs <> List.length BE.Regs.argRegs
            then raise Fail "BE.Spec.maxGPRArgs <> List.length BE.Regs.argRegs"
         else ()
      val _ =
         if BE.Spec.maxFPRArgs <> List.length BE.Regs.argFRegs
            then raise Fail "BE.Spec.maxFPRArgs <> List.length BE.Regs.argFRegs"
         else ()
      val mlStrm = BE.MLTreeComp.selectInstructions (BE.CFGGen.build ())
     (* extract operations from the emitter's streams *)
      val Stream.STREAM { beginCluster, getAnnotations, comment, emit, defineLabel, 
			  entryLabel, exitBlock, pseudoOp, endCluster, ...} = 
	  mlStrm
      val emit = fn stm => emit (annotate (stm, BE.MLTreeUtils.stmToString stm))
(*
if MChkTy.check stm
		        then emit (annotate (stm, BE.MLTreeUtils.stmToString stm))
		        else raise Fail ("incorrect type for MLRISC statement: "^
					 BE.MLTreeUtils.stmToString stm)
*)
      val emitStms = List.app emit
      val endCluster = BE.compileCFG o endCluster
		     
      val varDefTbl = BE.VarDef.newTbl ()
      val getDefOf = BE.VarDef.getDefOf varDefTbl
      val setDefOf = BE.VarDef.setDefOf varDefTbl
      val defOf = BE.VarDef.defOf varDefTbl
      val fdefOf = BE.VarDef.fdefOf varDefTbl
      val cdefOf = BE.VarDef.cdefOf varDefTbl
      val bind = BE.VarDef.bind varDefTbl

      fun flushLoads () = (case BE.VarDef.flushLoads varDefTbl
	  of [] => ()
	   | stms => (comment "flushLoads"; emitStms (List.rev stms)))

      val genGoto = BE.Transfer.genGoto varDefTbl
      val genPrim0 = Prim.genPrim0 {varDefTbl=varDefTbl}
      val genPrim = Prim.genPrim {varDefTbl=varDefTbl}

      (* literals *)
      val floatTbl = FloatLit.new ()
      val strTbl = StringLit.new ()
      val tagTbl = TagLit.new ()

      fun emitLit (l, p) = (
	  pseudoOp P.alignData;
	  defineLabel l;
	  pseudoOp p )

     (* use a special encoding for enums to distinguish them from pointers:
      * enum(e) => 2*e+1
      *)
      fun encodeEnum e = Word.<<(e, 0w1) + 0w1

      fun genLit (ty, Literal.Enum c) = 
	  MTy.EXP(ty, T.LI(T.I.fromWord (ty, encodeEnum c)))
	| genLit (ty, Literal.StateVal n) =
	      (* we want the two low bits of the state-value representation to be zero *)
	  MTy.EXP(ty, T.LI(T.I.fromWord (ty, Word.<<(n, 0w2))))
	| genLit (ty, Literal.Tag t) = let
	  val lbl = TagLit.addLit (tagTbl, t)
	  in
	      MTy.EXP (ty, T.LABEL lbl)
	  end
	| genLit (ty, Literal.Int i) = MTy.EXP(ty, T.LI i)
	| genLit (fty, Literal.Float f) = let
	  val lbl = FloatLit.addLit (floatTbl, (fty, f))
	  in
	      MTy.FEXP (fty, T.FLOAD (fty, T.LABEL lbl, ()))
	  end
	| genLit (_, Literal.Char c) = raise Fail "todo"
	| genLit (_, Literal.String s) = let
	  val lbl = StringLit.addLit (strTbl, s)
	  in
	      MTy.EXP (MTy.wordTy, T.LABEL lbl)
	  end

      (* Generate code for a control transfer, e.g., a function or a continuation call or a heap-limit check. *)
      fun genTransfer (M.StdApply args) = let
	  val {stms, liveOut} = (BE.Transfer.genStdApply varDefTbl args)
          in
             emitStms (annotateStms(stms, "stdApply"));
	     exitBlock liveOut
          end	 
	| genTransfer (M.StdThrow args) = let
	  val {stms, liveOut} = BE.Transfer.genStdThrow varDefTbl args
          in
	      emitStms (annotateStms (stms, "stdThrow"));
	      exitBlock liveOut
          end
	| genTransfer (M.Apply args) = let
          val {stms, liveOut} = BE.Transfer.genApply varDefTbl args
          in
	      emitStms (annotateStms (stms, "apply"));
	      exitBlock liveOut
          end
	| genTransfer (M.Goto jmp) = emitStms (annotateStms ((genGoto jmp), "goto"))
	| genTransfer (M.If (c, jT as (lT, argsT), jF)) = 
	  let val labT = newLabel "L_true"
	  in 
	      emit (T.BCC (cdefOf c, labT));
	      emitStms (genGoto jF);
	      defineLabel labT;
	      emitStms (genGoto jT)
	  end
	| genTransfer (M.Switch (v, cases, defaultCase)) = let		  
         (* put the switch value into reg *)
	  val reg = newReg()
	  val _ = emit(T.MV (MTy.wordTy, reg, defOf v))

	  (* compare the value with one of the cases *)
	  fun compareCase i = let
	      (* encode the case values according to the type of the switch value *)
	      val c = (case Var.typeOf v                      
			of CFGTy.T_Enum _ => 
			   (* enums use a special encoding, so we need to use it in cases *)
			   encodeEnum i
			 | _ => i
		      (* end case *))
	      in
	          T.CMP (MTy.wordTy, T.EQ, T.REG (MTy.wordTy, reg), T.LI(T.I.fromInt (MTy.wordTy, Word.toIntX c)))
	      end (* compareCase *)

         (* emit branching instructions; also return code that calls the exit functions
	  *)
	  fun genCase ((i, (l, [])), exits) = 
	      (* if the jump target has no arguments, put it directly into the branch instruction *) 
	      (emit (T.BCC (compareCase(i), BE.LabelCode.getName(l)));
	       exits)		      
	    | genCase ((i, handleCase), exits) = let
              val labT = newLabel("S_case")
	      in		
                 (* branching instructions *)
		  emit (T.BCC (compareCase(i), labT));
                 (* code for handling the case if it is chosen *)
		  (labT, genGoto(handleCase)) :: exits
	      end

	 (* exit the code block if the value equals the case *)
	  val exits = List.foldl genCase [] cases
         (* emit the default case *)
          val _ = Option.app (fn defJmp => emitStms (genGoto(defJmp))) defaultCase
		      
	  fun emitExit (labT, exitStms) = (
	      defineLabel(labT); 
	      emitStms(exitStms) )
	  in		  
	      List.app emitExit(List.rev(exits))
	  end
	| genTransfer (M.HeapCheck hc) = let
          val {stms, return} = BE.Transfer.genHeapCheck varDefTbl hc
	  in 
	      (* emit code for the heap-limit check and the transfer into the GC *) 
	      emitStms stms;
	      Option.app (fn (retLbl, retStms, liveOut) => (
		  (* emit code for the return function *)
		   emit (T.LIVE liveOut);
		   entryLabel retLbl;
		   emitStms retStms))
	         return
	  end
	| genTransfer (M.AllocCCall call) = emitStms (BE.Transfer.genAllocCCall varDefTbl call)
	      

      (* bind CFG variables to MLRISC trees.  we annotate MLRISC code with the given message. *)
      and bindExp (lhs, rhs, msg) = let
	  fun f (l, r) = emitStms (annotateStms(bind (l, r), msg))
          in
             ListPair.appEq f (lhs, rhs)
          end                 

      (* Construct an MLRISC tree from a CFG expression. *)
      and genExp frame = let
	  fun gen (M.E_Var(lhs, rhs)) = 
	      ListPair.app setDefOf (lhs, List.map getDefOf rhs)
	    | gen (M.E_Const(lhs, lit, _)) = 
	      bindExp ([lhs], [genLit (szOfVar lhs, lit)], "")
	    | gen (M.E_Label(lhs, l)) = 
	      bindExp ([lhs], [MTy.EXP (MTy.wordTy, (T.LABEL (BE.LabelCode.getName l)))], "")
	    | gen (M.E_Select(lhs, i, v)) = let
	      val rhs = BE.Alloc.select {lhsTy=Var.typeOf lhs, mty=Var.typeOf v, i=i, base=defOf v}
              in
		  bindExp ([lhs], [rhs], "let "^v2s lhs^" = "^v2s v^"["^i2s i^"]")
	      end
	    | gen (M.E_Update(i, lhs, rhs)) = let
              val szI = BE.Types.szOfIx (Var.typeOf lhs, i)
	      val wordSzB = IntInf.toInt Spec.ABI.wordSzB
	      val offset = T.LI (T.I.fromInt (MTy.wordTy, wordSzB * i))
	      in
		  flushLoads ();
		  emit(annotate(T.STORE (szI, T.ADD (MTy.wordTy, defOf lhs, offset), defOf rhs, ManticoreRegion.memory),
		       v2s lhs^" := "^v2s rhs))
	      end
	    | gen (M.E_AddrOf(lhs, i, v)) = let
	      val addr = BE.Alloc.tupleAddrOf {mty=Var.typeOf v, i=i, base=defOf v}
	      in
		  bindExp ([lhs], [MTy.EXP(MTy.wordTy, addr)], "addrof("^v2s v^"["^Int.toString i^"])")
	      end
	    | gen (M.E_Alloc (lhs, vs)) = let 
              val {ptr, stms} = BE.Alloc.genAlloc (List.map (fn v => (Var.typeOf v, getDefOf v)) vs)
	      in 
		  emitStms stms;
		  bindExp ([lhs], [ptr], "alloc "^v2s lhs^" = "^String.concat (List.map v2s vs))
	      end
	    | gen (M.E_GAlloc(lhs, vs)) = let 
              val {ptr, stms} = BE.Alloc.genGlobalAlloc (List.map (fn v => (Var.typeOf v, getDefOf v)) vs)
	      in 
		  emitStms (annotateStms (stms, "galloc "^v2s lhs^" = "^String.concat (List.map v2s vs)));
		  bindExp ([lhs], [ptr], "")
	      end
	    | gen (M.E_Promote (lhs, v)) =  let
              val {stms, result} = BE.Transfer.genPromote varDefTbl {lhs=lhs, arg=v}
	      in
		  emitStms stms;
		  bindExp ([lhs], result, "promote")
	      end
	    | gen (M.E_Prim0 p) = emitStms(annotateStms(genPrim0 p, PrimUtil.nameOf p))
	    | gen (M.E_Prim (lhs, p)) = emitStms(annotateStms(genPrim (lhs, p), PrimUtil.nameOf p))
	    | gen (M.E_CCall (lhs, f, args)) = let 
              val {stms, result} = BE.Transfer.genCCall varDefTbl {lhs=lhs, f=f, args=args}
	      in
		  emitStms stms;
		  bindExp (lhs, result, "ccall "^Var.toString f)
	      end
	    | gen (M.E_Cast(lhs, _, v)) = 
	      bindExp ([lhs], [getDefOf v], "")
	    (* vproc operations *)
	    | gen (M.E_HostVProc lhs) =
	      bindExp ([lhs], [BE.VProcOps.genHostVP], "host()")
	    | gen (M.E_VPLoad(lhs, offset, vproc)) =
	      bindExp ([lhs], [BE.VProcOps.genVPLoad varDefTbl (szOfVar lhs, offset, vproc)], "vpload "^v2s lhs)
	    | gen (M.E_VPStore(offset, vproc, v)) =
	      emitStms(annotateStms([BE.VProcOps.genVPStore varDefTbl (szOfVar v, offset, vproc, v)], "vpstore "^v2s v))
         in
	    gen
         end (* genExp *)

      (* The first function of the module is its entry point. The code generator tacks on the
       * module entry label, "mantEntry", to indicate where the RTS can execute the module.
       *)
      val entryFunc as M.FUNC{lab=entryLab, ...} :: _ = code
      val clusters = GenClusters.clusters code
		     
      fun genFunc (M.FUNC {lab, entry, body, exit}) = let
	  fun emitLabel () = let
	      val label = BE.LabelCode.getName lab
	      in
	         (* if this function is the module entry point, output the entry label *)
	         if M.Label.same (lab, entryLab)
                    then (pseudoOp (P.global RuntimeLabels.entry);  entryLabel RuntimeLabels.entry)
                    else (); 
		 (* output the label *)
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
			 | _ => entryLabel label)
		    | _ => raise Fail "emitLabel"
		 (* end case *))
	      end (* emitLabel *)		  
	  val stms = BE.Transfer.genFuncEntry varDefTbl (lab, entry)
	  (* finish a function by emitting the function body *)
	  fun finish () = let
	      val funcAnRef = getAnnotations ()
	      val frame = BE.SpillLoc.getFuncFrame lab
	      val regs = BE.LabelCode.getParamRegs lab
	      in	
	        if (Controls.get annotateInstrs)
		   then (List.app ((fn s => comment ("param: "^s)) o MTy.treeToString o MTy.regToTree) regs;
			 comment ("CFG function: "^CFG.Label.toString lab))
                   else ();
	        (* flush out any stale loads from other functions*)
	         BE.VarDef.flushLoads varDefTbl;
		 funcAnRef := (#create BE.SpillLoc.frameAn) frame :: (!funcAnRef);
		 emitLabel ();
		 emitStms stms;
		 List.app (genExp frame) body;
		 genTransfer exit
	      end (* finish *)
          in
	      finish
          end (* genFunc *)

      fun genCluster c = let
	  val _ = BE.VarDef.clear varDefTbl;
	  val finishers = List.map genFunc c
          in 
 	     beginCluster 0;
	     pseudoOp P.text;
	     List.app (fn f => f ()) finishers;
	     endCluster []
          end
	 
      fun genLiterals () = (
	  beginCluster 0;
	  pseudoOp P.rodata;	      
	 (* runtime constant magic number for sanity test *)
	  pseudoOp (P.global RuntimeLabels.magic);
	  defineLabel RuntimeLabels.magic;
	  pseudoOp (P.int (P.I32, [Spec.ABI.magic]));
	  FloatLit.appi (fn ((sz, f), l) => emitLit (l, P.float(sz, [f]))) floatTbl;
	  StringLit.appi (fn (s, l) => emitLit (l, P.asciz s)) strTbl;
         (* emit a dummy string label for tags *)
	  TagLit.appi (fn (t, l) => emitLit (l, P.asciz t)) tagTbl;
	  List.app emitLit (!BE.literals);
	  endCluster []
        )
    in
      Cells.reset ();	  
      List.app genCluster clusters;
      genLiterals () 
    end (* codeGen *) 

  val codeGen : {code: CFG.module, dst: TextIO.outstream} -> unit =
      BasicControl.mkTracePassSimple
	  {passName = "codeGen",
	   pass = codeGen}

end (* CodeGen *)
