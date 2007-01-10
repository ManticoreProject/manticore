(* codegen-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor CodeGenFn (??) =
  struct

  (* comment annotation on statements *)
    fun note (stm, msg) =
	  T.ANNOTATION(stm, #create MLRiscAnnotations.COMMENT msg)

    fun generate (??) = let
	(* extract operations from the emitter's streams *)
	  val Stream.STREAM{beginCluster, ...} = mlStrm
 	  val Stream.STREAM{getAnnotations, ...} = mlStrm
	  val Stream.STREAM{endCluster, ...} = mlStrm
	  val Stream.STREAM{comment, ...} = mlStrm
	  val endCluster = ??
	  val gen = let
		val Stream.STREAM{emit=gen, ...} = mlStrm
(* NOTE: we shoud probably also have a flag for debugging code generation
 * that dumps this to a debug file.
 *)
		val gen = if !CodegenOptions.debugInsnSelect
		      then fn stm =>
			gen(note(stm, BE.MLTreeUtils.stmToString stm))
		      else gen
		in
		  gen
		end
	  fun gen' [] = ()
	    | gen' (stm::r) = (gen stm; gen' r)
	  val Stream.STREAM{defineLabel, ...} = mlStrm
	  val Stream.STREAM{entryLabel, ...} = mlStrm
	  val Stream.STREAM{exitBlock, ...} = mlStrm
	  val Stream.STREAM{pseudoOp, ...} = mlStrm
	(* generate code for expressions *)
	  fun genExp (_, e) = (case e
		of CFG.E_Let(xs, rhs, e) =>
		 | CFG.E_HeapCheck(szb, callGC, e) =>
		 | CFG.E_If(cond, trueJmp, falseJmp) =>
		 | CFG.E_Switch(arg, cases, SOME dflt) =>
		 | CFG.E_Switch(arg, cases, NONE) =>
		 | CFG.E_Apply(f, args) =>
		 | CFG.E_Throw(k, args) =>
		 | CFG.E_Goto gmp = genJump jmp
		(* end case *))
	(* generate code for rhs of let *)
	  and genRHS ([x], CFG.E_Var y) =
	    | genRHS ([x], CFG.E_Label lab) =
		bindToRExp (vTbl, T.LABEL(LabelCode.getName lab))
	    | genRHS ([x], CFG.E_Literal lit) =
	    | genRHS ([x], CFG.E_Select(i, y)) =
	    | genRHS ([x], CFG.E_Alloc(ty, args)) =
	    | genRHS ([x], CFG.E_Prim p) = genPrim (vTbl, x, p)
	    | genRHS ([x], CFG.E_CCall(cfun, args)) =
	(* jump to local label *)
	  and genJump (lab, args, fallThrough) = let
		val name = LabelCode.getName lab
		val params = LabelCode.getParams lab
		val args = List.map getDef args
		in
		  Copy.copy {src = args, dst = params};
		  if fallThrough then () else gen (T.JMP(T.LABEL name, [name]))
		end
	  in
	    ??
	  end

  end
