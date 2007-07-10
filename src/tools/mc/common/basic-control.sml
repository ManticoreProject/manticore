(* basic-control.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu/)
 * All rights reserved.
 *
 * Basic controls for the mc compiler.
 *)

structure BasicControl :  sig

  (* the top-level registery of the compiler *)
    val topRegistry : ControlRegistry.registry

  (* nest a tier-2 registery within the top-level registery *)
    val nest : string * ControlRegistry.registry * Controls.priority -> unit

  (* wrap a pass 'pre -> 'post pass with debug output controled by a new
   * "keep" control.
   *)
    val mkPass : {
	    preOutput: TextIO.outstream * 'pre -> unit,
            preExt: string,
            postOutput: TextIO.outstream * 'post -> unit,
            postExt: string,
            passName: string,
            pass: 'pre -> 'post,
            registry: ControlRegistry.registry
	  } -> 'pre -> 'post

    val mkPassSimple : {
	    output: TextIO.outstream * 'a -> unit,
            ext: string,
            passName: string,
            pass: 'a -> 'a,
            registry: ControlRegistry.registry
	  } -> 'a -> 'a

   val debugObscurity : int

  (* *)
    val show_all : (string -> unit) -> 
                  (({ctl: string Controls.control,
                     info: ControlRegistry.control_info} -> string) *
                   ({ctl: string Controls.control,
                     info: ControlRegistry.control_info} -> string)) ->
                  int option -> unit

    val newRegistryWithDebug : {name : string, help : string}
	  -> (ControlRegistry.registry * bool Controls.control)

  end = struct

    val topRegistry = ControlRegistry.new {help = "mc controls"}

    fun nest (prefix, reg, pri) =
       ControlRegistry.nest topRegistry {prefix = SOME prefix,
                                         pri = pri,
                                         obscurity = 0,
                                         reg = reg}

    val debugObscurity = 2


    fun ('pre, 'post) mkPass {preOutput: TextIO.outstream * 'pre -> unit,
                              preExt: string,
                              postOutput: TextIO.outstream * 'post -> unit,
                              postExt: string,
                              passName: string,
                              pass: 'pre -> 'post,
                              registry: ControlRegistry.registry} : 'pre -> 'post =
       let
          val keepPassCtl =
             Controls.genControl
             {name = "keep-" ^ passName,
              pri = [5, 0],
              obscurity = 1,
              help = "keep " ^  passName ^ " passes",
              default = false}
          val _ = 
             ControlRegistry.register
             registry
             {ctl = Controls.stringControl ControlUtil.Cvt.bool keepPassCtl,
              envName = NONE}
          val countRef = ref 0
       in
          fn pre =>
          let
             val count = !countRef
             val () = countRef := count + 1
             val post =
        	if Controls.get keepPassCtl
                   then let
                           val outPre = 
                              TextIO.openOut (concat [passName, Int.toString count,
                                                      ".pre.", preExt])
                           val () = preOutput (outPre, pre)
                           val () = TextIO.closeOut outPre
                           val post = pass pre
                           val outPost = 
                              TextIO.openOut (concat [passName, Int.toString count,
                                                      ".post.", postExt])
                           val () = postOutput (outPost, post)
                           val () = TextIO.closeOut outPost
                         in
                           post
                	end
                   else pass pre
          in
             post
          end
       end
    fun mkPassSimple {output: TextIO.outstream * 'a -> unit,
                      ext: string,
                      passName: string,
                      pass: 'a -> 'a,
                      registry: ControlRegistry.registry} =
       mkPass {preOutput = output,
               preExt = ext,
               postOutput = output,
               postExt = ext,
               passName = passName,
               pass = pass,
               registry = registry}

    fun show_all output (getarg, getvalue) level =
       let
          fun walk indent (ControlRegistry.RTree rt) =
             let
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

    fun newRegistryWithDebug {name, help} = let
	  val newReg = ControlRegistry.new {help = help}
	  val priority = []
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
