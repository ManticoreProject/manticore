(* closure-fn.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor ClosureFn (Target : TARGET_SPEC) : sig

    val convert : CPS.module -> CFG.module

  end = struct

    structure SafeForSpaceClosures = ClosureConvertFn (Target)

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

    fun analyze {passName, pass} = BasicControl.mkTracePassSimple {
            passName = passName,
            pass = pass
          }
    val cfa = analyze {passName = "cfa", pass = CFACPS.analyze}
    val freeVars = analyze {passName = "freeVars", pass = FreeVars.analyze}

    structure ConvertStyle =
       struct
          datatype style = Flat | SafeForSpace
          fun toString Flat = "flat"
              | toString SafeForSpace = "safeForSpace"
          fun fromString "flat" = SOME Flat
              | fromString "safeForSpace" = SOME SafeForSpace
              | fromString _ = NONE
          val cvt = {
            tyName = "convertStyle",
            fromString = fromString,
            toString = toString
          }
       end

    val convertStyle = Controls.genControl {
            name = "convert-style",
            pri = [5, 0],
            obscurity = 1,
            help = "closure convert style (flat or safeForSpace)",
            default = ConvertStyle.Flat
          }

    val () = ControlRegistry.register ClosureControls.registry {
            ctl = Controls.stringControl ConvertStyle.cvt convertStyle,
            envName = NONE
          }

    fun doConvert module = (
      case Controls.get convertStyle
       of ConvertStyle.Flat => FlatClosureWithCFA.convert module
        | ConvertStyle.SafeForSpace => SafeForSpaceClosures.newConvert module
(* raise Fail "Safe-for-space closure conversion is not yet implemented" *)
      (* end case *))

    val convert' = BasicControl.mkKeepPass {
	    preOutput = PrintCPS.output,
            preExt = "cps",
            postOutput = PrintCFG.output {counts=false, types=false},
            postExt = "cfg",
            passName = "closure",
            pass = doConvert,
            registry = ClosureControls.registry
	  }

    fun convert module = let
        val _ = ClassifyConts.analyze module
        val _ = cfa module
        val _ = freeVars module 
    in
        convert' module
    end

  end
