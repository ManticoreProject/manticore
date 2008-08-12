(* mlb.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of MLB.
 *)

structure MLB : sig
    
  (* load the an MLB file *)
    val load : (Error.err_stream * string)
	  -> (Error.err_stream list * ProgramParseTree.PML1.program list)

  end = struct

    structure PT = MLBParseTree
    structure PPT = ProgramParseTree.PML1

    exception Error

(* FIXME: load files only once *)

  (* information for applying a preprocessor to a PML file.
   *)
    type preprocessor_cmd = (
	 string *		(* preprocessor directive, e.g., "preprocessor", for a generic *)
				(* preprocessor "cpp" for the C preprocessor *)
	 string *		(* directory to run the preprocessor *)
	 string option *	(* preprocessor command *)
	 string list)		(* arguments *)

    type parse_tree = (Error.err_stream * PPT.program)

  (* environment for an MLB file *)
    datatype mlb_env
      = Env of {
	    loc : Error.span,			(* location in the MLB file *)
	    pts : parse_tree list,		(* parse trees; must retain ordering *)
	    preprocs : preprocessor_cmd list	(* preprocessors *)
	  }

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

  (* get the available string data from the input stream *)
    fun input inStrm = let	    
	    fun lp (NONE, lines) = List.rev lines
	      | lp (SOME line, lines) = lp (TextIO.inputLine inStrm, line :: lines)
	    val lines = lp(TextIO.inputLine inStrm, [])
            in
	        TextIO.closeIn inStrm;
		lines
	    end

  (* input the entire file *)
    fun inputFile file = let
	  val inStrm = TextIO.openIn file
	  in
	    input inStrm
	  end

  (* send a list of strings to the output stream *)
    fun output (lines, outStrm) = let
	  fun lp [] = TextIO.closeOut outStrm
	    | lp (l :: ls) = (
		TextIO.output(outStrm, l);
		lp ls)
	  in
	    lp lines
	  end

  (* copy from the input to the output stream *)
    fun copy (inStrm, outStrm) = output(input inStrm, outStrm)

  (* remove a temporary file *)
    fun removeTmp tmp = if OS.FileSys.access (tmp, [OS.FileSys.A_READ, OS.FileSys.A_WRITE])
           then OS.FileSys.remove tmp
           else ()

  (* pass the file through a sequence of preprocessors *)
    fun chainPreprocs (file, path, name, []) = raise Fail "compiler bug"
      | chainPreprocs (file, path, name, ("cpp", dir, NONE, [includes]) :: ppCmds) = let
	(* special syntax for the C preprocessor *)
	  val includes = String.tokens (fn c => c = #",") includes
	  val args = 
	      (* include the current directory *)
		"-I." 
	      (* strip away preprocessor directives and cruft *)
		:: "-P" 
	      (* preset directive: the absolute file path *)
		:: concat["-DPML_PATH=", path, name]
	      (* preset directive: the file name *)
		:: ("-DPML_FILE="^name) 
	      (* add the includes *)
		:: List.map (fn inc => "-include"^inc) includes 
	      (* pass the file to cpp through stdin *)
		@ ["-"]
	  in
	    chainPreprocs(file, path, name, ("preprocess", dir, SOME RunCPP.cppCmd, args) :: ppCmds)
	  end
      | chainPreprocs (file, path, name, [("preprocess", dir, SOME ppCmd, args)]) = let
	  val ppProc = runPreproc(dir, ppCmd, args)
	  val outStrm = Unix.textOutstreamOf ppProc
	  val lines = inputFile file
	  in
	    removeTmp file;
	    output(lines, outStrm);
	    (Unix.textInstreamOf ppProc, mkReap ppProc)
	  end
      | chainPreprocs (file, path, name, ("preprocess", dir, SOME ppCmd, args) :: ppCmds) = let
	  val ppProc = runPreproc(dir, ppCmd, args)
	(* get the lines of the temporary file *)
	  val lines = inputFile file
	(* send those lines to the preprocessor's stdin *)
	  val _ = output(lines, Unix.textOutstreamOf ppProc)
	  val tmp = OS.FileSys.tmpName()
	  val file2 = TextIO.openOut tmp
	  in
	    copy(Unix.textInstreamOf ppProc, file2);
	    Unix.reap ppProc;
	    chainPreprocs(tmp, path, name, ppCmds)
	  end
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

    fun filePath file = Atom.atom(OS.FileSys.getDir()^Atom.toString file)
  
    fun alreadyDefined (env, file) = AtomMap.inDomain(env, filePath file)

  (* process a basis expression *)
    fun processBasExp (mlb, env as Env{loc, pts, preprocs}) = (case mlb
        of PT.MarkBasExp {span, tree} => 
	     processBasExp (tree, Env{loc=span, pts=pts, preprocs=preprocs})
	 | PT.DecBasExp basDec => 
	     processBasDec (basDec, env)
	 | _ => raise Fail "todo"
        (* end case *))

  (* process a basis declaration *)
    and processBasDec (basDec, env as Env{loc, pts, preprocs}) = (case basDec
        of PT.MarkBasDec {span, tree} => 
	   processBasDec (tree, Env{loc=span, pts=pts, preprocs=preprocs})
	 (* imports can be pml or mlb files *)
	 | PT.ImportBasDec file => (
	      case OS.Path.splitBaseExt (Atom.toString file)
	       of {base, ext=SOME "mlb"} => let
		      val pts' = loadMLB (file, Env{loc=loc, pts=pts, preprocs=preprocs})
		      in
		          pts'@pts
		      end
		| {base, ext=SOME "pml"} => (
                  case loadPML (file, Env{loc=loc, pts=pts, preprocs=preprocs})
		   of NONE => pts
		    | SOME (errStrm, pt) => (errStrm, pt) :: pts
                  (* end case *))
		| _ => raise Fail "unknown source file extension"
           (* end case *))
	 | PT.AnnBasDec("preprocess", ppCmd :: ppArgs, basDec) => let
	       val preproc = ("preprocess", OS.FileSys.getDir(), SOME ppCmd, ppArgs)
	       in
	         processBasDec(basDec, Env{loc=loc, pts=pts, preprocs=preproc :: preprocs})
	       end
	 | PT.AnnBasDec("cpp", includes, basDec) => let
	       val preproc = ("cpp", OS.FileSys.getDir(), NONE, includes)
	       in
	         processBasDec(basDec, Env{loc=loc, pts=pts, preprocs=preproc :: preprocs})
	       end
	 | PT.SeqBasDec basDecs =>
	   processBasDecs(basDecs, env)
	 | _ => raise Fail "todo"
        (* end case *))

    and processBasDecs (basDecs, env as Env{loc, pts, preprocs}) = 
	List.foldl 
	    (fn (basDec, pts) => processBasDec(basDec, Env{loc=loc, pts=pts, preprocs=preprocs}))
	    pts
	    basDecs

(* FIXME: check that the MLB file is valid *)

  (* load an MLB file *)
    and loadMLB (file, env as Env{loc, pts, preprocs}) = let
	  val fileStr = Atom.toString file
	  val errStrm = Error.mkErrStream fileStr
          in
	    case MLBParser.parseFile (errStrm, fileStr)
	     of SOME {span, tree=basDecs} => let
		  val dir = OS.FileSys.getDir()
		  val {dir=dir', ...} = OS.Path.splitDirFile fileStr
		  val dir' = OS.FileSys.fullPath dir'
		  val _ = OS.FileSys.chDir dir'
		  val pts = processBasDecs(basDecs, Env{loc=span, pts=pts, preprocs=preprocs})
		  in 
		    checkForErrors(#1(ListPair.unzip pts));
		    OS.FileSys.chDir dir;
		    pts
		  end
	      | NONE => (
		  checkForErrors[errStrm];
		  raise Fail "impossible")
	    (* end case *)
	  end

  (* load a PML file *)
    and loadPML (file, env as Env{loc, pts, preprocs}) = let 
	  val fileStr = Atom.toString file
	  val errStrm = Error.mkErrStream fileStr
	  val (inStrm, reap) = preprocess(List.rev preprocs, fileStr)
	  val ptOpt = Parser.parseFile (errStrm, inStrm)
	  in
	    reap();
	    case ptOpt
	     of SOME pt => SOME (errStrm, pt)
	      | NONE => (
		  checkForErrors[errStrm];
		  raise Fail "impossible")
	    (* end case *)
	  end

  (* load the MLB file *)
    fun load (errStrm, file) = let
	  val env0 = Env{loc=(0,0), pts=[], preprocs=[]}
	  val pts = loadMLB(Atom.atom file, env0)
	  in
	    ListPair.unzip (List.rev pts)
	  end

  end (* MLB *)
