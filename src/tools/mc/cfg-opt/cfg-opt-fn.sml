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
    fun analyze {passName, pass} = BasicControl.mkTracePassSimple {
            passName = passName,
            pass = pass
          }

    structure AddAllocChecks = AddAllocChecksFn (Target)
    structure AllocCCalls = AllocCCallsFn (Target)
    structure ImplementCalls = ImplementCallsFn (Target)

  (* wrap analysis passes *)
    val census = analyze {passName = "census", pass = Census.census}
    val cfa = analyze {passName = "cfa", pass = CFACFG.analyze}
  (* wrap transformation passes with keep controls *)
    val contract = transform {passName = "contract", pass = Contract.transform}
    val specialCalls = transform {passName = "specialize-calls", pass = SpecializeCalls.transform}
    val implCalls = transform {passName = "implement-calls", pass = ImplementCalls.transform}
    val allocChecks = transform {passName = "alloc-checks", pass = AddAllocChecks.transform}
    val allocCCalls = transform {passName = "alloc-c-calls", pass = AllocCCalls.transform}

    fun optimize module = let
          val _ = CheckCFG.check module
	  val _ = census module
	  val module = contract module
          val _ = CheckCFG.check module
	  val _ = cfa module
	  val module = specialCalls module
          val _ = CFACFG.clearInfo module
          val _ = CheckCFG.check module
          val module = implCalls module
          val _ = CheckCFG.check module
	  val _ = census module
	  val module = contract module
          val _ = CheckCFG.check module
	  val module = allocChecks module
          val _ = CheckCFG.check module
          val module = allocCCalls module
	  val _ = CheckCFG.check module
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
