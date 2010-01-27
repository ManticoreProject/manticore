(* closure.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Closure : sig

    val convert : CPS.module -> CFG.module

  end = struct

    fun convert module = (
	  ClassifyConts.analyze module;
          FlatClosureWithCFA.convert module
	  (* end case *))

    val convert = BasicControl.mkKeepPass {
	    preOutput = PrintCPS.output,
            preExt = "cps",
            postOutput = PrintCFG.output {counts=false, types=false},
            postExt = "cfg",
            passName = "closure",
            pass = convert,
            registry = ClosureControls.registry
	  }

  end
