(* bom-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor BOMOptFn (Spec : TARGET_SPEC) : sig

    val optimize : BOM.module -> BOM.module

  end = struct

  (* a wrapper for BOM optimization passes *)
    fun transform name xform = BasicControl.mkPassSimple {
	    output = PrintBOM.output,
	    ext = "bom",
	    passName = name,
	    pass = xform,
	    registry = BOMOptControls.registry
	  }

    val expand = BasicControl.mkPass {
	    preOutput = PrintBOM.output,
	    preExt = "bom",
	    postOutput = fn (out, NONE) => () 
			  | (out, SOME p) => PrintBOM.output (out, p),
	    postExt = "bom",
	    passName = "expand",
	    pass = ExpandHLOps.expand,
	    registry = BOMOptControls.registry
	  }

    val contract = transform "contract" Contract.contract

    fun expandAll module = (case expand module
	   of SOME module => expandAll (contract module)
	    | NONE => module
	  (* end case *))

    val uncurry = transform "uncurry" Uncurry.transform
    val caseSimplify = transform "caseSimplify" CaseSimplify.transform
    val expandAll = transform "expandAll" expandAll

    fun optimize module = let
	  val module = contract module
	  val module = uncurry module
	  val module = contract module
	  val module = expandAll module
	  val module = caseSimplify module
	  in
	    module
	  end

    val optimize = BasicControl.mkPassSimple {
	    output = PrintBOM.output,
	    ext = "bom",
	    passName = "optimize",
	    pass = optimize,
	    registry = BOMOptControls.registry
	  }

  end
