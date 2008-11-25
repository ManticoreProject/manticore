(* cps-opt-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor CPSOptFn (Spec : TARGET_SPEC) : sig

    val optimize : CPS.module -> CPS.module

  end = struct

  (* a wrapper for CPS optimization passes *)
    fun transform {passName, pass} = BasicControl.mkKeepPassSimple {
	    output = PrintCPS.output,
	    ext = "cps",
	    passName = passName,
	    pass = pass,
	    registry = CPSOptControls.registry
	  }
    fun analyze {passName, pass} = BasicControl.mkTracePassSimple {
            passName = passName,
            pass = pass
          }

  (* wrap analysis passes *)
    val census = analyze {passName = "census", pass = Census.census}
    val cfa = analyze {passName = "cfa", pass = CFACPS.analyze}

  (* wrap transformation passes with keep controls *)

    fun optimize module = let
	  val _ = census module
          val _ = cfa module
          val _ = CFACPS.clearInfo module
	  in
	    module
	  end

    val optimize = BasicControl.mkKeepPassSimple {
	    output = PrintCPS.output,
            ext = "cps",
            passName = "optimize",
            pass = optimize,
            registry = CPSOptControls.registry
	  }

  end
