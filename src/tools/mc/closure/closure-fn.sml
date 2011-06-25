(* closure-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor ClosureFn (Target : TARGET_SPEC) : sig

    val convert : CPS.module -> CFG.module

  end = struct

    structure ClosureConvert = ClosureConvertFn (Target)

    fun transform {passName, pass} = let
	  val xform = BasicControl.mkKeepPassSimple {
		  output = PrintCPS.output,
		  ext = "clos",
		  passName = passName,
		  pass = pass,
		  registry = ClosureControls.registry
		}
	  fun xform' module = let
		val module = xform module
		val _ = CheckCPS.check (passName, module)
		in
		  module
		end
	  in
	    xform'
	  end

    val closureConvert = transform {passName = "closure-convert", pass = ClosureConvert.transform} 

    fun analyze {passName, pass} = BasicControl.mkTracePassSimple {
            passName = passName,
            pass = pass
          }
    val cfa = analyze {passName = "cfa", pass = CFACPS.analyze}
    val freeVars = analyze {passName = "freeVars", pass = FreeVars.analyze}

    val convert' = BasicControl.mkKeepPass {
	    preOutput = PrintCPS.output,
            preExt = "cps",
            postOutput = PrintCFG.output {counts=false, types=false},
            postExt = "cfg",
            passName = "closure",
            pass = FlatClosureWithCFA.convert,
            registry = ClosureControls.registry
	  }

    fun convert module = let
        val _ = ClassifyConts.analyze module
        val _ = cfa module
        val module = closureConvert module
        val _ = freeVars module 
    in
        convert' module
    end

  end
