(* bom-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor BOMOptFn (Spec : TARGET_SPEC) : sig

    val optimize : BOM.module -> BOM.module

  end = struct

  (* a wrapper for BOM optimization passes.  The wrapper includes an invariant check. *)
    fun transform {passName, pass} = let
	  val xform = BasicControl.mkKeepPassSimple {
		  output = PrintBOM.output,
		  ext = "bom",
		  passName = passName,
		  pass = pass,
		  registry = BOMOptControls.registry
		}
	  fun xform' module = let
		val module = xform module
		val _ = CheckBOM.check (passName, module)
		in
		  module
		end
	  in
	    xform'
	  end

    fun moduleOptOutput (out, NONE) = ()
      | moduleOptOutput (out, SOME p) = PrintBOM.output (out, p)

    fun mkModuleOptPass (passName, pass) = BasicControl.mkKeepPass {
	    preOutput = PrintBOM.output,
	    preExt = "bom",
	    postOutput = moduleOptOutput,
	    postExt = "bom",
	    passName = passName,
	    pass = pass,
	    registry = BOMOptControls.registry
	  }

    val expand = mkModuleOptPass ("expand", ExpandHLOps.expand)

    val contract = transform {passName = "contract", pass = Contract.contract {removeExterns=true}}
    val expandAllContract = transform {passName = "expand-all-contract", pass = Contract.contract {removeExterns=false}}
    val rewriteAllContract = transform {passName = "rewrite-all-contract", pass = Contract.contract {removeExterns=false}}

    val rewrite = mkModuleOptPass ("rewrite", RewriteHLOps.rewrite)

    fun expandAll module = (case expand module
	   of SOME module => let
		val _ = CheckBOM.check ("expand-all:expand", module)
	      (* NOTE: we don't remove externs here because references may be hiding inside
	       * unexpanded HLOps.
	       *)
		val module = expandAllContract module
		val _ = CheckBOM.check ("expand-all:contract", module)
		in
		  expandAll module
		end
	    | NONE => module
	  (* end case *))

(* FIXME: rewriting and expansion should be interleaved!!! *)
    fun rewriteAll module = (case rewrite module
	   of SOME module => let
		val _ = CheckBOM.check ("rewrite-all:rewrite", module)
	      (* NOTE: we don't remove externs here because references may be hiding inside
	       * unexpanded HLOps.
	       *)
		val module = rewriteAllContract module
		val _ = CheckBOM.check ("rewrite-all:contract", module)
		in
		  rewriteAll module
		end
	    | NONE => module
	  (* end case *))

  (* the expansive inlining pass *)
    fun inline specializeRecFuns = let
	  val xform = BasicControl.mkKeepPassSimple {
		  output = PrintBOM.output,
		  ext = "bom",
		  passName = "inline",
		  pass = Inline.transform {specializeRecFuns=specializeRecFuns},
		  registry = BOMOptControls.registry
		}
	  fun xform' module = let
		val module = xform module
		val _ = CheckBOM.check ("inline", module)
		in
		  module
		end
	  in
	    xform'
	  end

    val groupFuns = transform {passName = "group-funs", pass = GroupFuns.transform}
    val uncurry = transform {passName = "uncurry", pass = Uncurry.transform}
    val caseSimplify = transform {passName = "case-simplify", pass = CaseSimplify.transform}
    val rewriteAll = transform {passName = "rewrite-all", pass = rewriteAll}
    val expandAll = transform {passName = "expand-all", pass = expandAll}

    fun optimize module = let
	  val module = contract module
	  val module = inline false module  
	  val module = contract module
  (* to re-enable rewrites, fix them so that they use inline BOM instead of the old hlop files. *)
(*          val module = rewriteAll module*)
	  val module = expandAll module
	(* NOTE: we cannot run groupFuns until after HLOp expansion, since it doesn't know the
	 * recursive dependencies of the HLOps.
	 *)
	  val module = groupFuns module
	  val module = inline true module
	  val module = contract module  
	  val module = uncurry module
	  val module = contract module
	  val module = caseSimplify module
	  val module = contract module
	  in
	    module
	  end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintBOM.output,
	    ext = "bom",
	    passName = "bom-optimize",
	    pass = optimize,
	    registry = BOMOptControls.registry
	  }

  end
