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
    val cfaClear = analyze {passName = "cfa-clear", pass = CFACPS.clearInfo}
    val freeVars = analyze {passName = "freeVars", pass = FreeVars.analyze}
    val freeVarsClear = analyze {passName = "freeVars-clear", pass = FreeVars.clear}
    val classify = analyze {passName = "classify-conts", pass = ClassifyConts.analyze}

    val wrapCaptures = transform {passName = "wrap-captures", pass = WrapCaptures.transform}
    val unifyNonret = transform {passName = "unify-nonret-sigs", pass = UnifyNonRetSigs.transform}

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


    val () = (ControlRegistry.register ClosureControls.registry {
                ctl = Controls.stringControl ConvertStyle.cvt convertStyle,
                envName = NONE
              })


    fun doConvert module = (
      case Controls.get convertStyle
       of ConvertStyle.Flat =>
            if (Controls.get BasicControl.direct)
                then DirectFlatClosureWithCFA.convert module
                else FlatClosureWithCFA.convert module

        | ConvertStyle.SafeForSpace => SafeForSpaceClosures.newConvert module
(* raise Fail "Safe-for-space closure conversion is not yet implemented" *)
      (* end case *))

    val convert' = BasicControl.mkKeepPass {
	    preOutput = PrintCPS.output,
            preExt = "cps",
            postOutput = PrintCFG.output {counts=false, types=PrintCFG.Full, preds=false},
            postExt = "cfg",
            passName = "closure",
            pass = doConvert,
            registry = ClosureControls.registry
	  }

    fun convert module = let
        val module = if Controls.get BasicControl.direct
                      then unifyNonret module
                      else module

        val () = classify module

        val module = if Controls.get BasicControl.direct
                      then let
                              val () = freeVarsClear module
                              val () = FreeVars.analyzeForWrapCaptures module
                          in
                              wrapCaptures module
                          end
                      else module

        val () = cfaClear module
        val () = cfa module

        val () = if Controls.get BasicControl.direct
                  then if WrapCaptures.correctlyWrapped module
                    then ()
                    else raise Fail "WrapCaptures pass produced invalid code.\n\tDebug with: -Ccps.debug=true -Cclos.keep-wrap-captures=true"
                  else ()

        val () = freeVarsClear module
        val () = freeVars module
    in
        convert' module
    end

  end
