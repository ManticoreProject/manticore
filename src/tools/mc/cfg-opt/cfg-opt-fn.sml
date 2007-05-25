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
	  val _ = Census.census module
	  val _ = CFACFG.analyze module
	  val module = SpecializeCalls.transform module
	  val module = AddAllocChecks.transform module
	  in
	    module
	  end

  end
