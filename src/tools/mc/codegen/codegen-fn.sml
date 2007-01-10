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
		 | CFG.E_Apply(f, args) =>
		 | CFG.E_Throw(k, args) =>
		 | CFG.E_Goto gmp = genJump jmp
		(* end case *))
	(* jump to local label *)
	  and genJump (lab, args) = let
		val name = Label.getName lab
		val params = Label.getParams lab
		val args = List.map getDef args
		in
		  Copy.copy {src = args, dst = params};
		  gen (T.JMP(T.LABEL name, [name]))
		end
	  in
	    ??
	  end

  end
