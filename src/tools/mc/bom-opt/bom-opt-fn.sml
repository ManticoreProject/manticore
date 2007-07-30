(* bom-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor BOMOptFn (Spec : TARGET_SPEC) : sig

    val optimize : BOM.module -> BOM.module

  end = struct

  (* a wrapper for BOM optimization passes *)
    fun transform name ext xform = BasicControl.mkPassSimple {
	    output = PrintBOM.output,
	    ext = ext,
	    passName = name,
	    pass = xform,
	    registry = BOMOptControls.registry
	  }

    val expand = BasicControl.mkPass {
	    preOutput = PrintBOM.output,
	    preExt = "bom",
	    postOutput = fn (out, NONE) => () 
			  | (out, SOME p) => PrintBOM.output (out, p),
	    postExt = "bom-hlop",
	    passName = "expandHLOps",
	    pass = ExpandHLOps.expand,
	    registry = BOMOptControls.registry
	  }

    fun expandAll module = (case expand module
	   of SOME module => expandAll (Contract.contract module)
	    | NONE => module
	  (* end case *))

    val contract = transform "Contract" "bom-opt1" Contract.contract
(*    val uncurry = transform "Uncurry" "bom-opt2" Uncurry.transform*)
    val caseSimplify = transform "CaseSimplify" "bom-opt3" CaseSimplify.transform
    val expandAll = transform "Expand" "bom-opt4" expandAll

    fun optimize module = let
	  val module = contract module
(*	  val module = uncurry module*)
	  val module = contract module
	  val module = expandAll module
	  val module = caseSimplify module
	  in
	    module
	  end

    val optimize = BasicControl.mkPassSimple {
	    output = PrintBOM.output,
	    ext = "bom-opt",
	    passName = "Optimize",
	    pass = optimize,
	    registry = BOMOptControls.registry
	  }

  end
