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
    structure IB = InitialBasis

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

    val exeFile = ref "a.out"

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm = (
	  Error.report (TextIO.stdErr, errStrm);
	  if Error.anyErrors errStrm
	    then OS.Process.exit OS.Process.failure
	    else ())

  (* check a list of error streams for errors *)
    fun checkListForErrors l = let
	  fun chk (errStrm, anyErrors) = (
		Error.report (TextIO.stdErr, errStrm);
		anyErrors orelse Error.anyErrors errStrm)
	  in
	    if (List.foldl chk false l)
	      then OS.Process.exit OS.Process.failure
	      else ()
	  end

    fun prHdr msg = print(concat["******************** ", msg,  " ********************\n"])

    fun boundVarChks (errStrms, bEnv, pts) = let
	  fun chk (errStrm, p1, (p2s, env)) =let
		val (p2, env) = BoundVariableCheck.check (errStrm, p1, env)
		in
		  (p2 :: p2s, env)
		end
	  val (pts, _) = ListPair.foldl chk ([], bEnv) (errStrms, pts)
	  in
	    List.rev pts
	  end

    fun tree {tree, span} = tree
    fun allDecls pts = List.concat(List.map tree pts)

  (* environment bootstrapping *)
    fun initialEnv () = let
	(* load the initial-basis.pml file *)
	  val initialBasisPath = OS.Path.joinDirFile{
		  dir = LoadPaths.sequentialBasisDir,
		  file = "initial-basis.mlb"
		}
	  val errStrm = Error.mkErrStream initialBasisPath
	  val [(_, initialPT)] = MLB.loadMLBWithoutBasis initialBasisPath
	(* bind variables and typecheck *)
	  val (initialPT, bEnv) = BoundVariableCheck.check (errStrm, initialPT, IB.primBindingEnv)
	  val _ = checkForErrors errStrm
	  val (mEnv, _, initialAST) = ChkModule.checkTopDecls errStrm
		((0, 0), tree initialPT, IB.primEnv, ModuleEnv.ModuleMap.empty)
	  val _ = checkForErrors errStrm
	(* set up the initial basis environments *)
	  val {bEnv, mEnv, glueAST} = IB.extendInitialEnv(bEnv, mEnv)
	  in
	    (bEnv, mEnv, initialAST, glueAST)
	  end

  (* dead function elimination on the parse tree *)
    fun printTrees ([]) = ()
      | printTrees ({tree,span}::rest) = (
        PrintPT.print tree;
        printTrees rest)
    fun treeShake p2s =
	  if Controls.get BasicControl.treeShake
	     then (
              if Controls.get BasicControl.treeShakeDebug
              then printTrees p2s
              else ();
	      TreeShake.setDeadFuns (allDecls p2s);
	      TreeShake.shakeProgram p2s)
	  else p2s

    val getPArrImpl : unit -> Types.tycon = DelayedBasis.TyCon.rope

  (* load the AST specified by an MLB file *)
    fun mlbToAST (errStrm, bEnv, mEnv, file) = let
        (* load the MLB file *)
	  val {basis, program} = MLB.load file
	  val _ = checkForErrors errStrm
        (* bound-variable check *)
	  fun chk ((errStrm, pt), (bEnv, errStrms, pts)) = let
		val (pt, bEnv) = BoundVariableCheck.check (errStrm, pt, bEnv)
		in
		  checkForErrors errStrm;
		  (bEnv, errStrm::errStrms, pt::pts)
		end
	  val (bEnv, errStrms, basis) = List.foldl chk (bEnv, [], []) basis
	(* record the basis binding environment *)
	  val _ = BasisEnv.saveBasisEnv bEnv
	  val (bEnv, errStrms, program) = List.foldl chk (bEnv, errStrms, basis) program
	  val p2s = treeShake (List.rev program)
        (* module and type checking *)
	  val ast = ChkProgram.check (mEnv, ListPair.zip(List.rev errStrms, p2s))
	  in
	    checkListForErrors errStrms;
	    ast
	  end

  (* the compiler's backend *)
    fun bomToCFG bom = let
	  val bom = BOMOpt.optimize bom	
          val cps = Convert.transform bom
	  val cps = CPSOpt.optimize cps
	  val cfg = Closure.convert cps
	  val cfg = CFGOpt.optimize cfg
	  in
	    cfg
	  end

    fun buildExe (verbose, asmFile) = let
	  val sts = BuildExecutable.build{
		  verbose = verbose,
		  asmFile = asmFile,
		  outFile = !exeFile
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
	    buildExe (verbose, outFile)
	  end (* compile *)

  (* compile an MLB or PML file *)
    fun mlbC (verbose, errStrm, srcFile, asmFile) = let
	  val _ = if verbose then print "initializing environment\n" else ()
	  val (bEnv0, mEnv0, ast0, glueAST) = initialEnv()
          val _ = if verbose then print(concat["mlton parsing \"", srcFile, "\"\n"]) else ()
          val sxml = Wrapper.compileSML (srcFile, asmFile)
	  val _ = if verbose then print(concat["parsing \"", srcFile, "\"\n"]) else ()
          val ast = mlbToAST (errStrm, bEnv0, mEnv0, srcFile)
          val _ = checkForErrors errStrm
          val ast = ASTOpt.optimize(glueAST(ast0, ast))
	  val _ = MatchCheck.checkExp (errStrm, ast)
	  val ast = MatchCompile.compile (errStrm, ast)
          val _ = checkForErrors errStrm
	(* create the initial translation environment *)
          val bom = Translate.translate (IB.primTranslationEnv, ast)
          val cfg = bomToCFG bom
	  in
	    codegen (verbose, asmFile, cfg);
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
	    mlbC (verbose, Error.mkErrStream file, file, OS.Path.joinBaseExt{base = base, ext = SOME "s"})
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
