(* cfg-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor CFGOptFn (Target : TARGET_SPEC) : sig

    val optimize : CFG.module -> CFG.module

  end = struct

  (* a wrapper for CFG optimization passes *)
    fun transform {passName, pass} = BasicControl.mkKeepPassSimple {
	    output = PrintCFG.output {types=true},
	    ext = "cfg",
	    passName = passName,
	    pass = pass,
	    registry = CFGOptControls.registry
	  }

    structure AddAllocChecks = AddAllocChecksFn (Target)
    structure ImplementCalls = ImplementCallsFn (Target)

  (* wrap transformation passes with keep controls *)
    val specialCalls = transform {passName = "specialize-calls", pass = SpecializeCalls.transform}
    val implCalls = transform {passName = "implement-calls", pass = ImplementCalls.transform}
    val allocChecks = transform {passName = "alloc-checks", pass = AddAllocChecks.transform}

    fun optimize module = let
          val () = CheckCFG.check module
	  val _ = Census.census module
	  val _ = CFACFG.analyze module
	  val module = specialCalls module
          val () = CheckCFG.check module
          val module = implCalls module
          val () = CheckCFG.check module
	  val module = allocChecks module
          val () = CheckCFG.check module
	  in
	    module
	  end


    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintCFG.output {types=true},
	    ext = "cfg",
	    passName = "cfg-optimize",
	    pass = optimize,
	    registry = CFGOptControls.registry
	  }

  end
