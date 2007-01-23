(* cfg-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor CFGOptFn (Target : TARGET_SPEC) : sig

    val optimize : CFG.module -> CFG.module

  end = struct

    structure AddAllocChecks = AddAllocChecksFn (Target)

    fun optimize module = (
	  CFACFG.analyze module;
	  AddAllocChecks.transform module)

  end
