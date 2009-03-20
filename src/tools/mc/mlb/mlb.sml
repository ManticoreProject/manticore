(* mlb.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of MLB.
 *)

structure MLB : sig
    
  (* load a PML or MLB file (including the basis library). *)
    val load : string -> {
	    basis : (Error.err_stream * ProgramParseTree.PML1.program) list,
	    program : (Error.err_stream * ProgramParseTree.PML1.program) list
	  }

  (* load an MLB file with the empty environment (i.e., no basis) *)
    val loadMLBWithoutBasis : string -> (Error.err_stream * ProgramParseTree.PML1.program) list

  end = struct

    structure PT = MLBParseTree
    structure PPT = ProgramParseTree.PML1

    exception Error

  (* information for applying a preprocessor to a PML file. *)
    type preprocessor_cmd = {
	directive : string,	(* preprocessor directive, e.g., "preprocessor", for
				 * a generic preprocessor "cpp" for the C 
				 * preprocessor
				 *)
	dir : string,		(* directory to run the preprocessor *)
	cmd : string option,	(* preprocessor command *)
	args : string list	(* command-line arguments *)
      }

    type parse_tree = (Error.err_stream * PPT.program)

  (* environment for an MLB file *)
    datatype mlb_env
      = Env of {
	   loc : Error.span,                           (* location in the MLB file *)
	   pts : parse_tree list,                      (* parse trees; must retain ordering *)
	   preprocs : preprocessor_cmd list            (* preprocessors *)
        }

    local
	val mlbs : AtomSet.set ref = ref AtomSet.empty
    in
    fun visitMLB (dir, file) = 
	  mlbs := AtomSet.add(!mlbs, Atom.atom (OS.Path.joinDirFile{dir=dir, file=file}))
    fun alreadyVisitedMLB (dir, file) = 
	  AtomSet.member(!mlbs, Atom.atom(OS.Path.joinDirFile{dir=dir, file=file}))
    end

    fun revConcat ls = List.concat (List.rev ls)
    fun concatMap f ls = List.concat (List.map f ls)

    local (* preprocessor support *)
    fun mkReap proc = fn () => ignore(Unix.reap proc)

  (* apply the preprocessor in a given directory *)
    fun runPreproc (dir', cmd, args) = let
	  val dir = OS.FileSys.getDir()
	  val _ = OS.FileSys.chDir dir'
	  val x = Unix.execute(cmd, args)
	  in
	      OS.FileSys.chDir dir;
	      x
	  end

  (* input all string data from the stream *)
    fun inputAll inStrm = let	    
	  fun lp (NONE, lines) = List.rev lines
	    | lp (SOME line, lines) = lp (TextIO.inputLine inStrm, line :: lines)
	  val lines = lp(TextIO.inputLine inStrm, [])
	  in
	      TextIO.closeIn inStrm;
	      lines
	  end

  (* input the entire file *)
    val inputFile = inputAll o TextIO.openIn

  (* output a list to the output stream *)
    fun outputAll (lines, outStrm) = let
	  fun lp [] = TextIO.closeOut outStrm
	    | lp (l :: ls) = (
		TextIO.output(outStrm, l);
		lp ls)
	  in
	      lp lines
	  end

  (* copy from the input to the output stream *)
    fun copy (inStrm, outStrm) = outputAll(inputAll inStrm, outStrm)

  (* remove a temporary file *)
    fun removeTmp tmp = if OS.FileSys.access (tmp, []) then OS.FileSys.remove tmp else ()

  (* parse the cpp definition string
   *   def1, ..., defn
   * where def can be a file, directory, or predef (prefixed with -D). we return
   * a list of files and directories, and a list of predefs.
   *)
    val parseCPPDefs = 
  	  List.partition (String.isPrefix "-D") o String.tokens (fn c => c = #",")

  (* parse a CPP predef, i.e., -DFOO=2 => ("FOO", SOME "2") *)
    fun parseCPPPredef predef = let
	  val predef = Substring.extract(predef, 2, NONE)
	  val (predef, def) = Substring.splitl (fn c => c <> #"=") predef
          in
	    if (Substring.isEmpty def)
	      then (Substring.string predef, NONE)
	      else (Substring.string predef, SOME(Substring.string(Substring.triml 1 def)))
	  end

  (* compute the list of #defines for the CPP based on controls *)
    fun defines () = let
	  fun add (true, d, l) = (d, NONE)::l
	    | add (false, d, l) = l
	  in
	    add (Controls.get BasicControl.sequential, "SEQUENTIAL",
	    add (Controls.get BasicControl.logging, "ENABLE_LOGGING",
	    add (not(Controls.get BasicControl.debug), "NDEBUG",
	      [])))
	  end

  (* pass the file through a sequence of preprocessors *)
    fun chainPreprocs (file, path, name, []) = raise Fail "compiler bug"
      | chainPreprocs (file, path, name, {directive="cpp", dir, cmd=NONE, args=[defs]} :: ppCmds) =
	  let
	  val (predefs, includes) = parseCPPDefs defs
	(* support for the C preprocessor *)
	  val includes = 
		"." ::
		LoadPaths.basisCPPDefDir ::
		includes
	  val predefs = List.map RunCPP.mkDef ([
		("PML_PATH", SOME (OS.FileSys.fullPath dir^"/"^name)),
		("PML_FILE", SOME name)
	      ] @ List.map parseCPPPredef predefs @ defines())
	  val args = RunCPP.mkArgs {relativeTo=dir, includes=includes, predefs=predefs, file=NONE}
	  in
	    chainPreprocs (
	      file, path, name,
	      {directive="preprocess", dir=dir, cmd=SOME RunCPP.cppCmd, args=args} :: ppCmds)
	  end
      | chainPreprocs (file, path, name, [{directive="preprocess", dir, cmd=SOME ppCmd, args}]) = let
          val ppProc = runPreproc(dir, ppCmd, args)
	  val outStrm = Unix.textOutstreamOf ppProc
	  val lines = inputFile file
	  in
	      removeTmp file;
	      outputAll(lines, outStrm);
	      (Unix.textInstreamOf ppProc, mkReap ppProc)
	  end
      | chainPreprocs (file, path, name, {directive="preprocess", dir, cmd=SOME ppCmd, args} :: ppCmds) =
	  let
	  val ppProc = runPreproc(dir, ppCmd, args)
       (* get the lines of the temporary file *)
	  val lines = inputFile file
       (* send those lines to the preprocessor's stdin *)
	  val _ = outputAll(lines, Unix.textOutstreamOf ppProc)
	  val tmp = OS.FileSys.tmpName()
	  val file2 = TextIO.openOut tmp
	  in
	      copy(Unix.textInstreamOf ppProc, file2);
	      Unix.reap ppProc;
	      chainPreprocs(tmp, path, name, ppCmds)
	  end
      | chainPreprocs _ = raise Fail "invalid preprocessor"
    in
  (* run preprocessors over a file *)
    fun preprocess ([], file) = (TextIO.openIn file, fn () => ())
      | preprocess (preprocs, file) = let
	  val tmp = OS.FileSys.tmpName()
	  val _ = copy(TextIO.openIn file, TextIO.openOut tmp)
	  val (strm, reap) = chainPreprocs(tmp, OS.Path.dir file, OS.Path.file file, preprocs)
	  val _ = removeTmp tmp
	  in	       
	     (strm, reap)
	  end
    end (* preprocessor support *)

  (* check for errors and report them if there are any *)
    fun checkForError errStrm = (
	  Error.report (TextIO.stdErr, errStrm);
	  Error.anyErrors errStrm)

    fun emptyError errStrm = Error.error(errStrm, [])
	  
  (* check for errors in a stack of error streams *)
    fun checkForErrors errStrms = let
        (* if an error occurs, report the error for any enclosing MLB files *)
	fun f ([], _) = ()
	  | f (strm :: strms, checkedStrms) = if checkForError strm
            then (List.app emptyError checkedStrms;
		  List.map checkForError checkedStrms;
		  raise Error)
            else f(strms, strm :: checkedStrms)
        in
	   f(List.rev errStrms, [])
        end 

  (* process a basis expression *)
    fun processBasExp (mlb, env as Env{loc, pts, preprocs}) = (
	  case mlb
           of PT.MarkBasExp {span, tree} => 
	      processBasExp (tree, Env{loc=span, pts=pts, preprocs=preprocs})
	    | PT.DecBasExp basDec => processBasDec (basDec, env)
	    | _ => raise Fail "todo"
          (* end case *))

  (* process a basis declaration *)
    and processBasDec (basDec, env as Env{loc, pts, preprocs}) = (
	  case basDec
           of PT.MarkBasDec {span, tree} => 
	      processBasDec (tree, Env{loc=span, pts=pts, preprocs=preprocs})
	    (* imports can be pml or mlb files *)
	    | PT.ImportBasDec file => (
	      case OS.Path.splitBaseExt (Atom.toString file)
	       of {base, ext=SOME "mlb"} => let
		    val pts' = loadMLB (Atom.toString file, Env{loc=loc, pts=[], preprocs=preprocs})
		    in
		      pts' @ pts
		    end
		| {base, ext=SOME "pml"} => (
                    case loadPML (Atom.toString file, Env{loc=loc, pts=[], preprocs=preprocs})
		     of NONE => pts
		      | SOME (errStrm, pt) => (errStrm, pt) :: pts
                    (* end case *))
		| _ => raise Fail "unknown source file extension"
              (* end case *))
	    | PT.AnnBasDec("preprocess", ppCmd :: ppArgs, basDec) => let
		val preproc = {
			directive = "preprocess",
			dir = OS.FileSys.getDir(),
			cmd = SOME ppCmd,
			args = ppArgs
		      }
	        in
	           processBasDec(basDec, Env{loc=loc, pts=pts, preprocs=preproc :: preprocs})
	         end
	    | PT.AnnBasDec ("cpp", includes, basDec) => let
		val preproc = {
			directive = "cpp",
			dir = OS.FileSys.getDir(),
			cmd = NONE,
			args = includes
		      }
		in
		   processBasDec(basDec, Env{loc=loc, pts=pts, preprocs=preproc :: preprocs})
		end
	    | PT.AnnBasDec ("expansion-opt", opts, basDec) => let
              (* expansion options *) 
		val expOpts = ExpansionOpts.fromStrings opts
		val pts' = processBasDec(basDec, Env{loc=loc, pts=[], preprocs=preprocs})
		fun addExpOpt (errStrm, {tree, span}) = let
		      fun add decl = PPT.ExpansionOptsDecl(expOpts, [decl])
		      in
		         (errStrm, {tree=List.map add tree, span=span})
		      end
	        in
		   List.map addExpOpt pts' @ pts
		end
	    | PT.SeqBasDec basDecs => processBasDecs(basDecs, env)
	    | _ => raise Fail "todo"
        (* end case *))

    and processBasDecs (basDecs, env as Env{loc, pts, preprocs}) = 
	  List.foldl 
	      (fn (basDec, pts) => processBasDec(basDec, Env{loc=loc, pts=pts, preprocs=preprocs}))
	      pts
	      basDecs

(* FIXME: check that the MLB file is valid *)

  (* load an MLB file *)
    and loadMLB (path, env as Env{loc, pts, preprocs}) = 
	  if OS.FileSys.access(path, [OS.FileSys.A_READ])
	     then let
	       val {dir=dirOfFile, file} = OS.Path.splitDirFile path
	       val dirOfFile = OS.FileSys.fullPath dirOfFile
	       val dir = OS.FileSys.getDir()
	       val errStrm = Error.mkErrStream path
	       in
		 case MLBParser.parseFile (errStrm, path)
		  of SOME{span, tree=basDecs} => 
		       if alreadyVisitedMLB(dirOfFile, file)
			 then pts (* already loaded the mlb file *)
			 else let
			   val _ = visitMLB(dirOfFile, file)
			  (* change the working directory to that of the file *)
			   val _ = OS.FileSys.chDir dirOfFile
			  (* process the contents of the MLB file *)
			   val pts = processBasDecs(basDecs, Env{loc=span, pts=pts, preprocs=preprocs})
			   in 
			     checkForErrors(#1(ListPair.unzip pts));
			   (* return to the original working directory *)
			     OS.FileSys.chDir dir;
			     pts
			   end
		   | NONE => (checkForErrors[errStrm]; pts)
	       end
	     else raise Fail ("MLB file "^OS.FileSys.getDir()^"/"^path^" does not exist")

  (* load a PML file *)
    and loadPML (file, env as Env{loc, pts, preprocs}) = let 
	  val errStrm = Error.mkErrStream file
	  val (inStrm, reap) = preprocess(List.rev preprocs, file)
	  val ptOpt = Parser.parseFile (errStrm, inStrm)
		     (*handle Fail s => raise Fail (file^": "^s)*)
	  in
	    reap();
	    case ptOpt
	     of SOME pt => SOME (errStrm, pt)
	      | NONE => (checkForErrors[errStrm]; NONE)
	  end

  (* load the basis library *)
    fun loadBasisLib env = if Controls.get BasicControl.sequential
          then let
          (* runtime library *)
            val runtimeBasisLibPts = loadMLB(LoadPaths.runtimeBasisLib, env)
          (* load the top-level scheduler *)
	    val topLevelSchedPts = loadMLB(LoadPaths.topLevelSchedLib (Controls.get BasicControl.scheduler), env)
            in
              topLevelSchedPts @ runtimeBasisLibPts
            end
          else let
	  (* implicit-threading library *)
	    val implicitThreadingPts = loadMLB(LoadPaths.implicitThreadingLib, env)
          (* load the top-level scheduler *)
	    val topLevelSchedPts = loadMLB(LoadPaths.topLevelSchedLib (Controls.get BasicControl.scheduler), env)
          (* load the default implicit-thread scheduler *)
	    val defImplicitThreadSchedPts = loadMLB(LoadPaths.defImplicitThreadSchedLib, env)
	  (* CML *)
	    val cmlPts = loadMLB(LoadPaths.cmlLib, env)
            in
	       cmlPts @ defImplicitThreadSchedPts @ topLevelSchedPts @ implicitThreadingPts
	    end

    val emptyEnv = Env{loc=(0,0), pts=[], preprocs=[]}

  (* load a PML or root MLB file *)
    fun load file = let
	  val basis = loadBasisLib emptyEnv
	  val pts = (case OS.Path.splitBaseExt file
		 of {base, ext=SOME "mlb"} => loadMLB(file, emptyEnv)
		  | {base, ext=SOME "pml"} => [Option.valOf(loadPML(file, emptyEnv))]
		  | _ => raise Fail "unknown source file extension"
		(* end case *))
          in {
	    basis = List.rev basis,
	    program = List.rev pts
	  } end

  (* load an MLB file with the empty environment (i.e., no basis) *)
    fun loadMLBWithoutBasis file = List.rev (loadMLB(file, emptyEnv))

  end (* MLB *)
