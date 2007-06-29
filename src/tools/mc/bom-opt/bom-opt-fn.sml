(* bom-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor BOMOptFn (Spec : TARGET_SPEC) : sig

    val optimize : BOM.module -> BOM.module

  end = struct

    val contract = Contract.contract
    val expandAll = ExpandHLOps.expandAll
    val caseSimplify = CaseSimplify.transform

    fun optimize module = let
	  val module = contract module
	  val module = expandAll module
	  val module = caseSimplify module
	  in
	    module
	  end

    val optimize =
       BasicControl.mkPassSimple
       {output = PrintBOM.output,
        ext = "bom",
        passName = "BOMOptimize",
        pass = optimize,
        registry = BOMOptControls.registry}

  end
