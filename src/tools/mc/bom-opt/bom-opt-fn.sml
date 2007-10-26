(* bom-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor BOMOptFn (Spec : TARGET_SPEC) : sig

    val optimize : BOM.module -> BOM.module

  end = struct

  (* a wrapper for BOM optimization passes.  The wrapper includes a invariant check. *)
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

    val expand = BasicControl.mkKeepPass {
	    preOutput = PrintBOM.output,
	    preExt = "bom",
	    postOutput = fn (out, NONE) => () | (out, SOME p) => PrintBOM.output (out, p),
	    postExt = "bom",
	    passName = "expand",
	    pass = ExpandHLOps.expand,
	    registry = BOMOptControls.registry
	  }

    val contract = transform {passName = "contract", pass = Contract.contract}

    fun expandAll module = (case expand module
	   of SOME module => let
		val _ = CheckBOM.check ("expand-all:expand", module)
		val module = contract module
		val _ = CheckBOM.check ("expand-all:contract", module)
		in
		  expandAll module
		end
	    | NONE => module
	  (* end case *))

    val uncurry = transform {passName = "uncurry", pass = Uncurry.transform}
    val inline = transform {passName = "inline", pass = Inline.transform}
    val caseSimplify = transform {passName = "case-simplify", pass = CaseSimplify.transform}
    val expandAll = transform {passName = "expand-all", pass = expandAll}

    fun optimize module = let
	  val module = contract module
	  val module = uncurry module
	  val module = contract module
	  val module = inline module  
	  val module = contract module
	  val module = expandAll module
	  val module = inline module
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
