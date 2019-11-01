(* basic-control.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Basic controls for the pmlc compiler.
 *)

structure BasicControl :  sig

  (* the top-level registery of the compiler *)
    val topRegistry : ControlRegistry.registry

  (* nest a tier-2 registery within the top-level registery *)
    val nest : string * ControlRegistry.registry * Controls.priority -> unit

  (* base name for pass output files; set based on compilation unit. *)
    val keepPassBaseName : string option Controls.control

  (* custom arguments to LLVM's opt *)
    val customOPT : string list Controls.control

  (* custom arguments to LLVM's llc *)
    val customLLC : string list Controls.control

  (* verbosity of diagnostics. *)
    val verbose : int Controls.control

  (* sequential mode *)
    val sequential : bool Controls.control

  (* use the LLVM backend *)
    val llvm : bool Controls.control

  (* use the direct-style codegen with contiguous stacks *)
    val direct : bool Controls.control

  (* same as "direct", but use segmented stacks instead *)
    val segstack : bool Controls.control

  (* same as "direct", but use resizing stacks instead *)
    val resizestack : bool Controls.control

  (* same as "direct", but use a linked-frame stack instead *)
    val linkstack : bool Controls.control

  (* switch to a seperate stack for all C calls. *)
    val cshim : bool Controls.control

  (* generate code for the native CPU. *)
    val native : bool Controls.control

  (* no return-address stack; emit pop/push jmps instead *)
    val noras : bool Controls.control

    val lazyunderflow : bool Controls.control
    val nosealingcapture : bool Controls.control

  (* optimization level used by LLVM backend *)
    val llopt : int Controls.control

  (* select the top-level thread scheduler *)
    val scheduler : string Controls.control

  (* disable parray basis *)
    val noparray : bool Controls.control

  (* keep GC header table, etc. *)
    val keepTemps : bool Controls.control

  (* link with debug version of runtime mode *)
    val debug : bool Controls.control

  (* enable collection of GC and memory statistics *)
    val gcStats : bool Controls.control

  (* enable logging mode *)
    val logging : bool Controls.control

  (* enable hw perf counters mode *)
    val perf : bool Controls.control

  (* maximum leaf size in ropes *)
    val maxLeafSize : int Controls.control

  (* perform dead function elimination on the parse tree *)
    val treeShake : bool Controls.control

  (* perform dead function elimination on the parse tree *)
    val treeShakeDebug : bool Controls.control

  (* wrap a 'pre -> 'post pass with a tracing diagnostic, controled by the
   * "verbose" control.
   *)
    val mkTracePass : {
            passName: string,
            pass: 'pre -> 'post,
            verbose: int
	  } -> 'pre -> 'post
    val mkTracePassSimple : {
            passName: string,
            pass: 'pre -> 'post
	  } -> 'pre -> 'post

  (* wrap a 'pre -> 'post pass with debug output controled by a new
   * "keep" control.  The pass is also traced (as with mkTracePass).
   *)
    val mkKeepPass : {
	    preOutput: TextIO.outstream * 'pre -> unit,
            preExt: string,
            postOutput: TextIO.outstream * 'post -> unit,
            postExt: string,
            passName: string,
            pass: 'pre -> 'post,
            registry: ControlRegistry.registry
	  } -> 'pre -> 'post

    val mkKeepPassSimple : {
	    output: TextIO.outstream * 'a -> unit,
            ext: string,
            passName: string,
            pass: 'a -> 'a,
            registry: ControlRegistry.registry
	  } -> 'a -> 'a

   val debugObscurity : int

  (* *)
    val showAll : (string -> unit) ->
                  (({ctl: string Controls.control,
                     info: ControlRegistry.control_info} -> string) *
                   ({ctl: string Controls.control,
                     info: ControlRegistry.control_info} -> string)) ->
                  int option -> unit

    val newRegistryWithDebug : {name : string, help : string, pri : int}
	  -> (ControlRegistry.registry * bool Controls.control)

  end = struct

    val topRegistry = ControlRegistry.new {help = "pmlc controls"}

    fun nest (prefix, reg, pri) = ControlRegistry.nest topRegistry {
	    prefix = SOME prefix,
	    pri = pri,
	    obscurity = 0,
	    reg = reg
	  }

    val debugObscurity = 2

    val keepPassBaseName : string option Controls.control = Controls.genControl {
	    name = "keepPassBaseName",
	    pri = [5, 0],
	    obscurity = debugObscurity + 1,
	    help = "",
	    default = NONE
	  }

    val customOPT : string list Controls.control = Controls.genControl {
	    name = "customOPT",
	    pri = [0, 0],
	    obscurity = 0,
	    help = "comma-separated argument(s) to pass to LLVM's `opt` instead of pre-defined ones",
	    default = []
	  }

    val customLLC : string list Controls.control = Controls.genControl {
	    name = "customLLC",
	    pri = [0, 0],
	    obscurity = 0,
	    help = "comma-separated argument(s) to pass to LLVM's `llc` instead of pre-defined ones",
	    default = []
	  }

    val verbose : int Controls.control = Controls.genControl {
	    name = "verbose",
	    pri = [0, 0],
	    obscurity = 0,
	    help = "controls verbosity of debugging messages",
	    default = 0
	  }

    val maxLeafSize : int Controls.control = Controls.genControl {
            name = "max-leaf-size",
            pri = [0, 0],
            obscurity = 0,
            help = "sets the upper bound on number of data items at leaves of ropes",
            default = 256
          }

    val treeShake : bool Controls.control = Controls.genControl {
            name = "tree-shake",
            pri = [0, 0],
            obscurity = 0,
            help = "dead function elimination on the parse tree (dead functions do not get type checked)",
            default = true
          }

    val treeShakeDebug : bool Controls.control = Controls.genControl {
            name = "tree-shake-debug",
            pri = [0, 0],
            obscurity = 0,
            help = "debug tree-shake",
            default = false
          }

  (* sequential mode *)
    val sequential : bool Controls.control = Controls.genControl {
	    name = "sequential",
	    pri = [0, 1, 1],
	    obscurity = 0,
	    help = "compile sequential programs",
	    default = false
	  }

  (* direct-style *)
    val direct : bool Controls.control = Controls.genControl {
        name = "direct",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "use direct-style code generation (with contiguous stacks)",
        default = false
      }

  (* segmented stacks *)
    val segstack : bool Controls.control = Controls.genControl {
        name = "segstack",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "use segmented stacks (uses direct-style conversion)",
        default = false
      }

  (* resizing stacks *)
    val resizestack : bool Controls.control = Controls.genControl {
        name = "resizestack",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "use resizing stacks (variant of segmented stacks)",
        default = false
      }

  (* mutable, linked-frame stack *)
    val linkstack : bool Controls.control = Controls.genControl {
        name = "linkstack",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "use mutable, linked-frame stacks (uses direct-style conversion)",
        default = false
      }

  (* use seperate stack for C calls *)
    val cshim : bool Controls.control = Controls.genControl {
        name = "cshim",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "use a seperate stack for C calls.",
        default = false
      }

  (* native instruction selection *)
    val native : bool Controls.control = Controls.genControl {
        name = "native",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "use native CPU instructions instead of generic x86_64",
        default = false
      }

  (* no return-address stack *)
    val noras : bool Controls.control = Controls.genControl {
        name = "noras",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "emit pop/push jmp instead of call/ret for stack management",
        default = false
      }

    val lazyunderflow : bool Controls.control = Controls.genControl {
        name = "lazyunderflow",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "segstack -- do not automatically free on underflow",
        default = false
      }

    val nosealingcapture : bool Controls.control = Controls.genControl {
        name = "nosealingcapture",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "segmented / resizing stack -- if true, does not seal the segment on callec",
        default = true
      }


  (* llvm backend *)
    val llvm : bool Controls.control = Controls.genControl {
        name = "llvm",
        pri = [0, 1, 1], (* TODO: What do these values mean? *)
        obscurity = 0,
        help = "use the LLVM backend for code generation",
        default = true
      }

    val llopt : int Controls.control = Controls.genControl {
            name = "llopt",
            pri = [0, 1, 1], (* TODO: What do these values mean? *)
            obscurity = 0,
            help = "choose optimization level (0 to 5) used by LLVM backend",
            default = 0
          }

  (* disable parray basis *)
    val noparray : bool Controls.control = Controls.genControl {
	    name = "noparray",
	    pri = [0, 1, 1],
	    obscurity = 0,
	    help = "disable parray basis inclusion",
	    default = false
	  }

(* keep header table, etc. *)
  val keepTemps : bool Controls.control = Controls.genControl {
      name = "keepTemps",
      pri = [0, 1, 1],
      obscurity = 0,
      help = "keep temporary files generated during compilation",
      default = false
    }

  (* link with debug version of runtime mode *)
    val debug : bool Controls.control = Controls.genControl {
	    name = "debug",
	    pri = [0, 1, 2],
	    obscurity = 0,
	    help = "include debugging support",
	    default = false
	  }

  (* select the top-level thread scheduler *)
    val scheduler : string Controls.control = Controls.genControl {
	    name = "scheduler",
	    pri = [0, 1, 2],
	    obscurity = 0,
	    help = "select the top-level thread-scheduler (round-robin or work-stealers)",
	    default = "round-robin"
	  }

  (* enable collection of GC and memory statistics *)
    val gcStats : bool Controls.control = Controls.genControl {
	    name = "gcstats",
	    pri = [0, 1, 3],
	    obscurity = 0,
	    help = "enable collection of GC statistics",
	    default = false
	  }

  (* enable logging mode *)
    val logging : bool Controls.control = Controls.genControl {
	    name = "log",
	    pri = [0, 1, 4],
	    obscurity = 0,
	    help = "enable logging of event history",
	    default = false
	  }

 (* enable hw perf counter mode *)
    val perf : bool Controls.control = Controls.genControl {
	    name = "perf",
	    pri = [0, 1, 4],
	    obscurity = 0,
	    help = "enable hardware performance counter tracking",
	    default = false
	  }

   val () = (
	  ControlRegistry.register topRegistry {
	      ctl = Controls.stringControl ControlUtil.Cvt.int verbose,
	      envName = NONE
	    };
	  ControlRegistry.register topRegistry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool sequential,
	      envName = NONE
	    };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool llvm,
          envName = NONE
        };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool direct,
          envName = NONE
        };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool segstack,
          envName = NONE
        };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool resizestack,
          envName = NONE
        };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool linkstack,
          envName = NONE
        };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool cshim,
          envName = NONE
        };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool native,
          envName = NONE
        };
      ControlRegistry.register topRegistry {
          ctl = Controls.stringControl ControlUtil.Cvt.bool noras,
          envName = NONE
        };
	  ControlRegistry.register topRegistry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool debug,
	      envName = NONE
	    };
	  ControlRegistry.register topRegistry {
	      ctl = Controls.stringControl ControlUtil.Cvt.string scheduler,
	      envName = NONE
	    };
	  ControlRegistry.register topRegistry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool logging,
	      envName = NONE
	    };
	  ControlRegistry.register topRegistry {
	      ctl = Controls.stringControl ControlUtil.Cvt.bool perf,
	      envName = NONE
	    };
          ControlRegistry.register topRegistry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool perf,
              envName = NONE
            };
          ControlRegistry.register topRegistry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool treeShake,
              envName = NONE
            };
          ControlRegistry.register topRegistry {
              ctl = Controls.stringControl ControlUtil.Cvt.bool treeShakeDebug,
              envName = NONE
            };
        ControlRegistry.register topRegistry {
            ctl = Controls.stringControl ControlUtil.Cvt.int llopt,
            envName = NONE
            };
          ControlRegistry.register topRegistry {
              ctl = Controls.stringControl ControlUtil.Cvt.int maxLeafSize,
              envName = NONE
          };
          ControlRegistry.register topRegistry {
              ctl = Controls.stringControl ControlUtil.Cvt.stringList customOPT,
              envName = NONE
          };
          ControlRegistry.register topRegistry {
              ctl = Controls.stringControl ControlUtil.Cvt.stringList customLLC,
              envName = NONE
          };
            ()
            )


    local
      val indent = ref 0
      val verboseCtl = verbose
    in
    val push = fn () => indent := !indent + 4
    val pop = fn () => indent := !indent - 4
    val say = fn s => (
	  print (CharVector.tabulate (!indent, fn _ => #" "));
	  print s;
	  print "\n")

    fun ('pre, 'post) mkTracePass {
	  passName : string,
	  pass : 'pre -> 'post,
	  verbose : int
	} = let
	  fun trace pre = let
		val msg = Controls.get verboseCtl >= verbose
                val inclusiveStart = Time.now()
		in
		  if msg
		    then (push (); say (concat [passName, " starting"]))
		    else ();
(***** NOTE: intercepting the exception here breaks the backtrace monitor.
		  (pass pre handle exn => (
		    say (concat [passName, " raised exception ", exnName exn]);
		    if msg then pop () else ();
		    raise exn)
		  ) before
*)
		  (pass pre) before
		    (if msg
		      then (let
                                val inclusive = Time.-(Time.now(), inclusiveStart)
                            in
                                say (concat [passName, " finished in: ", (Time.toString inclusive), "s (inclusive)"]);
                                pop ()
                            end)
		      else ())
		end
	  in
	    trace
	  end
    fun mkTracePassSimple {passName: string, pass: 'pre -> 'post} =
	  mkTracePass {passName = passName, pass = pass, verbose = 1}
    end (* local *)

  (* open an output file while reporting it on stdout *)
    fun openOut filename = let
	  val outS = TextIO.openOut filename
	  in
	    say(concat["dumping info to ", filename]);
	    outS
	  end

    fun ('pre, 'post) mkKeepPass {
	  preOutput : TextIO.outstream * 'pre -> unit,
	  preExt : string,
	  postOutput : TextIO.outstream * 'post -> unit,
	  postExt : string,
	  passName : string,
	  pass : 'pre -> 'post,
	  registry : ControlRegistry.registry
	} : 'pre -> 'post = let
          val keepPassCtl = Controls.genControl {
		name = "keep-" ^ passName,
		pri = [5, 0],
		obscurity = 1,
		help = concat["keep ",  passName, " passes"],
		default = false
	      }
          val _ = ControlRegistry.register registry {
		ctl = Controls.stringControl ControlUtil.Cvt.bool keepPassCtl,
                envName = NONE
	      }
          val countRef = ref 0
	  fun wrap pre = let
		val count = !countRef
		val () = countRef := count + 1
		val pass = mkTracePassSimple {passName = passName, pass = pass}
		val post = if Controls.get keepPassCtl
		      then let
			val fileName = (case Controls.get keepPassBaseName
			       of NONE => concat [passName, Int.toString count]
				| SOME baseName => concat [
				      baseName, ".", passName, Int.toString count
				    ]
			      (* end case *))
			val outPre = openOut (concat [fileName, ".pre.", preExt])
			val () = preOutput (outPre, pre)
			val () = TextIO.closeOut outPre
			val post = pass pre
			val outPost = openOut (concat [fileName, ".post.", postExt])
			val () = postOutput (outPost, post)
			val () = TextIO.closeOut outPost
			in
			  post
			end
		      else pass pre
		in
		  post
		end
	  in
	    wrap
	  end

    fun mkKeepPassSimple {
	  output: TextIO.outstream * 'a -> unit,
	  ext: string,
	  passName: string,
	  pass: 'a -> 'a,
	  registry: ControlRegistry.registry
	} = mkKeepPass {
	      preOutput = output,
	      preExt = ext,
	      postOutput = output,
	      postExt = ext,
	      passName = passName,
	      pass = pass,
	      registry = registry
	    }

    fun showAll output (getarg, getvalue) level = let
          fun walk indent (ControlRegistry.RTree rt) = let
        	val sp = CharVector.tabulate (indent, fn _ => #" ")
        	val sp' = CharVector.tabulate (indent + 1, fn _ => #" ")
        	val {help, ctls, subregs, path} = rt
        	fun one ci =
                   let
                      val arg = concat (foldr (fn (s, r) => s :: "." :: r)
                                              [getarg ci]
                                              path)
                      val value = getvalue ci
                   in
                      output (concat [sp',arg," : ",value,"\n"])
                   end
             in
        	(output (concat [sp, help, ":\n"])
                 ; app one ctls
                 ; app (walk (indent + 2)) subregs)
             end
       in
          walk 2 (ControlRegistry.controls (topRegistry, level))
       end

    fun newRegistryWithDebug {name, help, pri} = let
	  val newReg = ControlRegistry.new {help = help}
	  val priority = [pri]
	  val _ = nest (name, newReg, priority)
	  val debugCtl = Controls.genControl {
		  name = "debug",
		  pri = priority,
		  obscurity = debugObscurity,
		  help = "debug",
		  default = false
		}
	  in
	    ControlRegistry.register newReg {
	        ctl = Controls.stringControl ControlUtil.Cvt.bool debugCtl,
		envName = NONE
	      };
            (newReg, debugCtl)
	  end

   end
