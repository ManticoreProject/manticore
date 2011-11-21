(* cfg-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor CFGOptFn (Target : TARGET_SPEC) : sig

    val optimize : CFG.module -> CFG.module

  end = struct

  (* a wrapper for CFG optimization passes.  The wrapper includes an invariant check. *)
    fun transform {passName, pass} = let
	  val xform = BasicControl.mkKeepPassSimple {
		  output = PrintCFG.output {counts=true, types = false},
		  ext = "cfg",
		  passName = passName,
		  pass = pass,
		  registry = CFGOptControls.registry
		}
	  fun xform' module = let
		val module = xform module
		val _ = CheckCFG.check (passName, module)
		in
		  module
		end
	  in
	    xform'
	  end

    fun analyze {passName, pass} = BasicControl.mkTracePassSimple {
            passName = passName,
            pass = pass
          }

    structure AddAllocChecks = AddAllocChecksFn (Target)
    structure AllocCCalls = AllocCCallsFn (Target)
    structure AddAllocVecChecks = AddAllocVecChecksFn (Target)
    structure ImplementCalls = ImplementCallsFn (Target)

  (* wrap analysis passes *)
    val census = analyze {passName = "census", pass = Census.census}
    val cfa = analyze {passName = "cfa", pass = CFACFG.analyze}
    val cfaClear = CFACFG.clearInfo
  (* wrap transformation passes with keep controls *)
    val contract = transform {passName = "contract", pass = Contract.transform}
    val unrollLoops = transform {passName = "unroll-loops", pass = UnrollLoops.transform}
    val specialCalls = transform {passName = "specialize-calls", pass = SpecializeCalls.transform}
    val implCalls = transform {passName = "implement-calls", pass = ImplementCalls.transform}
    val allocChecks = transform {passName = "alloc-checks", pass = AddAllocChecks.transform}
    val allocCCalls = transform {passName = "alloc-c-calls", pass = AllocCCalls.transform}
    val allocVecChecks = transform {passName = "alloc-vec-checks", pass = AddAllocVecChecks.transform}

    fun optimize module = let
	  val _ = census module
	  val _ = CheckCFG.check ("closure", module)
	  val module = contract module
          val _ = cfa module
          val module = unrollLoops module
          val _ = cfaClear module
          val _ = cfa module
          val module = specialCalls module
          val _ = cfaClear module
          val module = implCalls module
	  val _ = census module
	  val module = contract module
	  val _ = cfa module
	  val module = allocChecks module
          val _ = cfaClear module
          val module = allocVecChecks module 
	  in
	    module
	  end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintCFG.output {counts=true, types=false},
	    ext = "cfg",
	    passName = "cfg-optimize",
	    pass = optimize,
	    registry = CFGOptControls.registry
	  }

  end
