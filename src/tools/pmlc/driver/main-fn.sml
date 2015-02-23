(* main-fn.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor MainFn (

    structure Spec : TARGET_SPEC
(* NEW-BOM *
    structure CG : CODE_GEN
*)

  ) : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    val _ = (
        SMLofNJ.Internals.TDP.mode := true;
        Coverage.install ();
        BackTrace.install ())

    structure Version = VersionFn (Spec)
(* NEW-BOM *
    structure BOMOpt = BOMOptFn (Spec)
    structure CPSOpt = CPSOptFn (Spec)
    structure CFGOpt = CFGOptFn (Spec)
    structure Closure = ClosureFn (Spec)
*)

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

    val exeFile = ref "a.out"

    fun frontEnd srcFile = (case OS.Path.ext srcFile
	   of SOME "mlb" => PMLFrontEnd.compileMLB {input = srcFile}
	    | SOME "pml" => PMLFrontEnd.compilePML {input = [srcFile]}
	    | SOME "sml" => PMLFrontEnd.compilePML {input = [srcFile]}
	    | SOME "sig" => PMLFrontEnd.compilePML {input = [srcFile]}
	    | SOME "fun" => PMLFrontEnd.compilePML {input = [srcFile]}
	    | _ => raise Fail "unknown file type"
	  (* end case *))

  (* compile an MLB or PML file *)
    fun mlbC (verbose, srcFile, asmFile) = let
	  val _ = if verbose then print "initializing environment\n" else ()
	  val _ = PMLFrontEnd.init ()
          val _ = if verbose then print(concat["mlton parsing \"", srcFile, "\"\n"]) else ()
          val sxml = frontEnd srcFile
(* NEW-BOM *
          val cfg = bomToCFG bom
*)
	  in
(* NEW-BOM *
	    codegen (verbose, asmFile, cfg);
*)
	    Stats.report ()
	  end

    fun doFile file = BackTrace.monitor (fn () => let
	  val verbose = (Controls.get BasicControl.verbose > 0)
	  val {base, ext} = OS.Path.splitBaseExt file
	  in
            case Controls.get BasicControl.keepPassBaseName
	     of NONE => Controls.set (BasicControl.keepPassBaseName, SOME base)
	      | SOME _ => ()
	    (* end case *);
	    mlbC (verbose, file, OS.Path.joinBaseExt{base = base, ext = SOME "s"})
	  end)

    fun quit b = OS.Process.exit (if b then OS.Process.success else OS.Process.failure)

    fun bad s = (
	  err s; 
          err "!* try `-h' or `-h<level>' for help\n";
          quit false)

    fun version () = (errnl Version.banner; quit true)

    val usageMsg = "\
	  \usage: pmlc [options] file\n\
          \\n\
          \  file:\n\
          \    <file>.pml\n\
          \    <file>.mlb\n\
          \\n\
          \  options:\n\
          \    -C<control>=<v>  set named control\n\
	  \    -o <file>        specify executable-file name\n\
          \    -H               produce complete help listing\n\
          \    -h               produce minimal help listing\n\
          \    -h<level>        help listing with obscurity limit\n\
          \    -version         show version\n\
	  \    -log             build an executable with logging enabled\n\
	  \    -gcstats         build an executable with GC statistics enabled\n\
	  \    -debug           build an executable with debugging enabled\n\
	  \    -perf            build an executable with hw perf counters enabled\n\
	  \    -sequential      compile a sequential-mode program\n\
	  \    -verbose         compile in verbose mode\n\
	  \"

    fun message (level, b) = (
	  err usageMsg;
	  if level = NONE
             then  ()
             else (
		err "\n";
                BasicControl.showAll err
                  (Controls.name o #ctl,
                    fn ci => concat [
			"(", #help (Controls.info (#ctl ci)), 
                        "; ", Controls.get (#ctl ci), ")"
		      ])
                  (valOf level));
          quit b)

    fun usage () = message (NONE, false)

    fun help level = message (level, true)

    fun processControl arg = let
          val spec = Substring.extract (arg, 2, NONE)
          val (name, value) =
             Substring.splitl (fn c => c <> #"=") spec
          val name = Substring.string name
          val names = String.fields (fn c => c = #".") name
          val value = if Substring.size value > 0
                then Substring.string (Substring.slice (value, 1, NONE))
        	else ""
	  in
            if name = "" orelse value = ""
              then bad (concat ["!* ill-formed -C option: `", arg, "'\n"])
              else (case ControlRegistry.control BasicControl.topRegistry names
		 of NONE => bad (concat ["!* unknown control: ",name,"\n"])
                  | SOME sctl => (
		      Controls.set (sctl, value)
                	handle Controls.ValueSyntax vse =>
                          bad (concat ["!* unable to parse value `",
                              value, "' for ", name, " : ", #tyName vse, "\n"
			    ]))
		(* end case *))
	   end

    fun processArgs args = (case args
           of arg :: args =>
		if String.isPrefix "-" arg
		  then processOption (arg, args)
		  else processFile (arg, args)
            | _ => usage ()
	  (* end case *))

    and processFile (arg, args) = (case (arg, args)
	   of (file, []) => (doFile file; quit true)
            | _ => usage ()
	  (* end case *))

    and processOption (arg, args) = let
	  fun badopt () = bad (concat ["!* ill-formed option: `", arg, "'\n"])
	  fun set ctl = (Controls.set(ctl, true); processArgs args)
	  in
            if String.isPrefix "-C" arg
	      then (processControl arg; processArgs args)
            else if String.isPrefix "-h" arg
	      then let
		val level = String.extract (arg, 2, NONE)
		in
		  if level = "" 
		    then help NONE
		  else if CharVector.all Char.isDigit level
		    then help (SOME (Int.fromString level))
		    else badopt ()
		end
            else (case arg
	       of "-o" => (case args
		     of exe::r => (exeFile := exe; processArgs r)
		      | _ => badopt()
		    (* end case *))
		| "-H" => help (SOME NONE)
		| "-version" => version ()
		| "-sequential" => set BasicControl.sequential
		| "-verbose" => (Controls.set(BasicControl.verbose, 1); processArgs args)
		| "-log" => set BasicControl.logging
		| "-gcstats" => set BasicControl.gcStats
		| "-debug" => set BasicControl.debug
		| "-perf" => set BasicControl.perf
		| _ => badopt ()
	      (* end case *))
	  end

    fun main (_, args) = processArgs args
 
  end

