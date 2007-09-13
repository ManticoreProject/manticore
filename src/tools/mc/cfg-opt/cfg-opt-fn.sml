(* cfg-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor CFGOptFn (Target : TARGET_SPEC) : sig

    val optimize : CFG.module -> CFG.module

  end = struct

    structure AddAllocChecks = AddAllocChecksFn (Target)

    fun optimize module = let
          val () = CheckCFG.check module
	  val _ = Census.census module
	  val _ = CFACFG.analyze module
	  val module = SpecializeCalls.transform module
          val () = CheckCFG.check module
          val module = ImplementCalls.transform module
          val () = CheckCFG.check module
	  val module = AddAllocChecks.transform module
          val () = CheckCFG.check module
	  in
	    module
	  end

    val optimize =
       BasicControl.mkKeepPassSimple
       {output = PrintCFG.output {types=true},
        ext = "cfg",
        passName = "CFGOptimize",
        pass = optimize,
        registry = CFGOptControls.registry}

  end
