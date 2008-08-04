(* mlb.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of MLB.
 *)

structure MLB : sig
    
  (* load the an MLB file *)
    val load : (Error.err_stream * string) -> (Error.err_stream list * ProgramParseTree.PML1.program list)

  end = struct

    structure PT = MLBParseTree
    structure PPT = ProgramParseTree.PML1

    exception Error

(* FIXME: load files only once *)

  (* path to the preprocessor executable and command-line arguments *)
    type preprocessor_cmd = (string * string list)

    type parse_tree = (Error.err_stream * PPT.program)

  (* environment for an MLB file *)
    datatype mlb_env
      = Env of 
             {
		loc : Error.span,                           (* location in the MLB file *)
		pts : parse_tree list,                      (* parse trees; must retain ordering *)
		preprocs : preprocessor_cmd list            (* preprocessors *)
              }

    fun revConcat ls = List.concat (List.rev ls)
    fun concatMap f ls = List.concat (List.map f ls)

    local (* preprocessor support *)
    fun mkReap proc = fn () => ignore(Unix.reap proc)

    fun input inStrm = let	    
	    fun lp (NONE, lines) = List.rev lines
	      | lp (SOME line, lines) = lp (TextIO.inputLine inStrm, line :: lines)
	    val lines = lp(TextIO.inputLine inStrm, [])
            in
	        TextIO.closeIn inStrm;
		lines
	    end

    fun inputFile file = let
	    val inStrm = TextIO.openIn file
            in
	        input inStrm
	    end

    fun output (lines, outStrm) = let
	    fun lp [] = TextIO.closeOut outStrm
	      | lp (l :: ls) = (
		  TextIO.output(outStrm, l);
		  lp ls)
            in
	        lp lines
	    end

    fun copy (inStrm, outStrm) = output(input inStrm, outStrm)

    fun tmpFile file = file ^ ".tmp-preproc"

    fun removeTmp tmp = if OS.FileSys.access (tmp, [OS.FileSys.A_READ, OS.FileSys.A_WRITE])
           then OS.FileSys.remove tmp
           else ()

    fun chainPreprocs (file, []) = raise Fail ""
      | chainPreprocs (file, [(ppCmd, args)]) = let
	    val ppProc = Unix.execute(ppCmd, args)
	    val outStrm = Unix.textOutstreamOf ppProc
	    val lines = inputFile file
	    val _ = removeTmp file
	    in
	        output(lines, outStrm);
		(Unix.textInstreamOf ppProc, mkReap ppProc)
	    end
      | chainPreprocs (file, (ppCmd, args) :: ppCmds) = let
	    val ppProc = Unix.execute(ppCmd, args)
         (* get the lines of the temporary file *)
	    val lines = inputFile file
         (* send those lines to the preprocessor's stdin *)
	    val _ = output(lines, Unix.textOutstreamOf ppProc)
	    val file2 = TextIO.openOut(file^"2")
	    in
	        copy(Unix.textInstreamOf ppProc, file2);
		ignore(Unix.reap ppProc);
		removeTmp file;
		chainPreprocs(file^"2", ppCmds)
	    end
    in
  (* pass the file through several preprocessors *)
    fun preprocess ([], file) = (TextIO.openIn file, fn () => ())
      | preprocess (preprocs, file) = let
	    val tmp = OS.FileSys.getDir()^"/"^(tmpFile file)
	    val _ = copy(TextIO.openIn file, TextIO.openOut tmp)
	    val (strm, reap) = chainPreprocs(tmp, preprocs)
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
	 | PT.AnnBasDec("preprocess", ppCmd :: ppArgs, basDec) =>
	   processBasDec(basDec, Env{loc=loc, pts=pts, preprocs=(ppCmd, ppArgs) :: preprocs})
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
	      (case MLBParser.parseFile (errStrm, fileStr)
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
               (* end case *))
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
           end

  (* load the MLB file *)
    fun load (errStrm, file) = let
	    val env0 = Env{loc=(0,0), pts=[], preprocs=[]}
	    val pts = loadMLB(Atom.atom file, env0)
            in
	        ListPair.unzip (List.rev pts)
            end

  end (* MLB *)
