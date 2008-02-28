(* closure.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Closure : sig

    val convert : CPS.module -> CFG.module

  end = struct

    structure ConvertStyle =
       struct
          datatype t = Flat | FlatWithCFA
          val toString = 
             fn Flat => "flat"
              | FlatWithCFA => "flatWithCFA"
          val fromString =
             fn "flat" => SOME Flat
              | "flatWithCFA" => SOME FlatWithCFA
              | _ => NONE
          val cvt = {tyName = "convertStyle",
                     fromString = fromString,
                     toString = toString}
       end
    datatype convertStyle = datatype ConvertStyle.t

    val convertStyle = Controls.genControl {
            name = "convert-style",
            pri = [5, 0],
            obscurity = 1,
            help = "closure convert style",
            default = Flat
          }
    val () = ControlRegistry.register ClosureControls.registry {
            ctl = Controls.stringControl ConvertStyle.cvt convertStyle,
            envName = NONE
          }

    val convert = fn cps =>
       case Controls.get convertStyle of
          Flat => FlatClosure.convert cps
        | FlatWithCFA => FlatClosureWithCFA.convert cps

    val convert = BasicControl.mkKeepPass {
	    preOutput = PrintCPS.output,
            preExt = "cps",
            postOutput = PrintCFG.output {types=true},
            postExt = "cfg",
            passName = "closure",
            pass = convert,
            registry = ClosureControls.registry
	  }
  end
