(* bom-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor BOMOptFn (Spec : TARGET_SPEC) : sig

    val optimize : BOM.module -> BOM.module

  end = struct

  (* a wrapper for BOM optimization passes *)
    fun transform {passName, pass} = 
       BasicControl.mkKeepPassSimple 
       {output = PrintBOM.output,
        ext = "bom",
        passName = passName,
        pass = pass,
        registry = BOMOptControls.registry}

    val expand = 
       BasicControl.mkKeepPass 
       {preOutput = PrintBOM.output,
        preExt = "bom",
        postOutput = fn (out, NONE) => () | (out, SOME p) => PrintBOM.output (out, p),
        postExt = "bom",
        passName = "expand",
        pass = ExpandHLOps.expand,
        registry = BOMOptControls.registry}

    val contract = transform {passName = "contract", pass = Contract.contract}

    fun expandAll module = (case expand module
	   of SOME module => let
                                val _ = CheckBOM.check module
                                val module = contract module
                                val _ = CheckBOM.check module
                             in
                                expandAll module
                             end
	    | NONE => module
	  (* end case *))

    val uncurry = transform {passName = "uncurry", pass = Uncurry.transform}
    val caseSimplify = transform {passName = "caseSimplify", pass = CaseSimplify.transform}
    val expandAll = transform {passName = "expandAll", pass = expandAll}

    fun optimize module = let
          val _ = CheckBOM.check module
	  val module = contract module
          val _ = CheckBOM.check module
	  val module = uncurry module
          val _ = CheckBOM.check module
	  val module = contract module
          val _ = CheckBOM.check module
	  val module = expandAll module
          val _ = CheckBOM.check module
	  val module = caseSimplify module
          val _ = CheckBOM.check module
	  in
	    module
	  end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintBOM.output,
	    ext = "bom",
	    passName = "BOMOptimize",
	    pass = optimize,
	    registry = BOMOptControls.registry
	  }

  end
