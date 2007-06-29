(* cps-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor CPSOptFn (Spec : TARGET_SPEC) : sig

    val optimize : CPS.module -> CPS.module

  end = struct

    fun optimize module = module

    val optimize =
       BasicControl.mkPassSimple
       {output = PrintCPS.output,
        ext = "cps",
        passName = "CPSOptimize",
        pass = optimize,
        registry = CPSOptControls.registry}

  end
