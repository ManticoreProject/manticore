(* main-fn.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

functor MainFn (

    structure Spec : TARGET_SPEC
    structure CG : CODE_GEN

  ) : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    val _ = (
        SMLofNJ.Internals.TDP.mode := true;
        Coverage.install ();
        BackTrace.install ())

    structure Version = VersionFn (Spec)
    structure BOMOpt = BOMOptFn (Spec)
    structure CPSOpt = CPSOptFn (Spec)
    structure CFGOpt = CFGOptFn (Spec)

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

    exception Error

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm = (
	  Error.report (TextIO.stdErr, errStrm);
	  if Error.anyErrors errStrm
	    then raise Error
	    else ())

    fun prHdr msg = print(concat["******************** ", msg,  " ********************\n"])

    fun srcToAST (errStrm, file) = let
          val astMaybe = FrontEnd.load (errStrm, file)
          val _ = checkForErrors errStrm
          in
            valOf astMaybe
          end

  (* the compiler's backend *)
    fun bomToCFG bom = let
	  val bom = BOMOpt.optimize bom	
          val _ = CheckBOM.check ("bom-optimize", bom)
          val cps = Convert.transform bom
	  val _ = CheckCPS.check ("convert", cps)
	  val cps = CPSOpt.optimize cps
	  val _ = CheckCPS.check ("cps-optimize", cps)
	  val cfg = Closure.convert cps
	  val _ = CheckCFG.check cfg
	  val cfg = CFGOpt.optimize cfg
	  val _ = CheckCFG.check cfg
	  in
	    cfg
	  end

    fun buildExe (verbose, asmFile, exeFile) = let
	  val exeFile = Option.getOpt (exeFile, "a.out")
	  val sts = BuildExecutable.build{
		  verbose = verbose,
		  asmFile = asmFile,
		  outFile = exeFile
		}
	  in
	    if OS.Process.isSuccess sts
	      then ()
	      else err "error compiling generated assembly code\n"
	  end

    fun codegen (verbose, outFile, cfg) = let
	  val outStrm = TextIO.openOut outFile
	  fun doit () = CG.codeGen {dst=outStrm, code=cfg}
	  in	  
	    AsmStream.withStream outStrm doit ();
	    TextIO.closeOut outStrm;
	    buildExe (verbose, outFile, NONE)
	  end (* compile *)

    fun bomC (verbose, errStrm, bomFile, asmFile) = let
	  val bom = BOMParser.parse (errStrm, bomFile)
          val _ = checkForErrors errStrm;
          val cfg = bomToCFG (valOf bom)
	  in
	    codegen (verbose, asmFile, cfg)
	  end

    fun mantC (verbose, errStrm, srcFile, asmFile) = let
          val ast = srcToAST(errStrm, srcFile)
          val _ = checkForErrors errStrm
          val ast = ASTOpt.optimize ast
	  val ast = MatchCompile.compile (errStrm, ast)
          val _ = checkForErrors errStrm
          val bom = Translate.translate ast
          val _ = CheckBOM.check ("translate", bom)
          val cfg = bomToCFG bom
	  in
	    codegen (verbose, asmFile, cfg)
	  end

    fun doFile file = BackTrace.monitor (fn () => let
	  val verbose = (Controls.get BasicControl.verbose > 0)
          fun doit compFn base = (
		case Controls.get BasicControl.keepPassBaseName
		 of NONE => Controls.set (BasicControl.keepPassBaseName, SOME base)
		  | SOME _ => ()
		(* end case *);
		compFn (
		  verbose,
		  Error.mkErrStream file,
		  file,
		  OS.Path.joinBaseExt {base = base, ext = SOME "s"}))
	  in
	    case OS.Path.splitBaseExt file
	     of {base, ext=SOME "bom"} => doit bomC base
	      | {base, ext=SOME "pml"} => doit mantC base
	      | _ => raise Fail "unknown source file extension"
	    (* end case *)
	  end)

    fun quit b = OS.Process.exit (if b then OS.Process.success else OS.Process.failure)

    fun bad s = (
	  err s; 
          err "!* try `-h' or `-h<level>' for help\n";
          quit false)

    fun version () = (errnl Version.banner; quit true)

    val usageMsg = "\
	  \usage: mc [options] file\n\
          \\n\
          \  file:\n\
          \    <file>.pml\n\
          \    <file>.bom\n\
          \\n\
          \  options:\n\
          \    -C<control>=<v>  (set named control)\n\
          \    -H               (produce complete help listing)\n\
          \    -h               (produce minimal help listing)\n\
          \    -h<level>        (help listing with obscurity limit)\n\
          \    -version         (show version)\n"

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
		if String.size arg > 0 andalso String.sub (arg, 0) = #"-"
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
            else if arg = "-H"
               then help (SOME NONE)
            else if arg = "-version"
               then version ()
            else badopt ()
	  end

    fun main (_, args) = (processArgs args) handle Error => OS.Process.failure
 
  end
