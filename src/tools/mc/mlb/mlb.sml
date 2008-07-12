(* mlb.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of MLB.
 *)

structure MLB : sig
    
  (* load the compilation unit of an MLB file *)
    val load : (Error.err_stream * string) -> (Error.err_stream list * ProgramParseTree.PML1.program list)

  end = struct

    structure PT = MLBParseTree
    structure PPT = ProgramParseTree.PML1

    exception Error

  (* path to the preprocessor executable and command-line arguments *)
    type preprocessor_cmd = (string * string list)

  (* environment for an MLB file *)
    datatype mlb_env
      = Env of 
             {
		loc : Error.span,                           (* location in the MLB file *)
		errStrms : Error.err_stream list,           (* error streams *)
		pts : PPT.program list,                     (* parse trees (we must have |pts| = |errStrms) *)
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
    end

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
    fun processBasExp (mlb, env as Env{loc, errStrms, pts, preprocs}) = (case mlb
        of PT.MarkBasExp {span, tree} => 
	     processBasExp (tree, Env{loc=span, errStrms=errStrms, pts=pts, preprocs=preprocs})
	 | PT.DecBasExp basDec => 
	     processBasDec (basDec, env)
	 | _ => raise Fail "todo"
        (* end case *))

  (* process a basis declaration *)
    and processBasDec (basDec, env as Env{loc, errStrms, pts, preprocs}) = (case basDec
        of PT.MarkBasDec {span, tree} => 
	   processBasDec (tree, Env{loc=span, errStrms=errStrms, pts=pts, preprocs=preprocs})
	 (* imports can be pml or mlb files *)
	 | PT.ImportBasDec file => let
           val file = Atom.toString file
	   val errStrms = Error.mkErrStream file :: errStrms
           in
	      case OS.Path.splitBaseExt file
	       of {base, ext=SOME "mlb"} => let
		      val (errStrms', pts') = loadMLB (file, Env{loc=loc, errStrms=errStrms, pts=pts, preprocs=preprocs})		      
		      in
		          (errStrms'@errStrms, pts'@pts)
		      end
		| {base, ext=SOME "pml"} => (
                  case loadPML (file, Env{loc=loc, errStrms=errStrms, pts=pts, preprocs=preprocs})
		   of NONE => (errStrms, pts)
		    | SOME (errStrm, pt) => (errStrm :: errStrms, pt :: pts)
                  (* end case *))
		| _ => raise Fail "unknown source file extension"
           end
	 | PT.AnnBasDec("preprocess", ppCmd :: ppArgs, basDec) =>
	   processBasDec(basDec, Env{loc=loc, errStrms=errStrms, pts=pts, preprocs=(ppCmd, ppArgs) :: preprocs})
	 | _ => raise Fail "todo"
        (* end case *))

  (* load an MLB file *)
    and loadMLB (file, env as Env{loc, errStrms, pts, preprocs}) = let
	  val errStrm = Error.mkErrStream file
          in
	      (case MLBParser.parseFile (errStrm, file)
		of SOME {span, tree=basDecs} => let
		       val dir = OS.FileSys.getDir()
		       val {dir=dir', ...} = OS.Path.splitDirFile file
		       val dir' = OS.FileSys.fullPath dir'
		       val _ = OS.FileSys.chDir dir'
		       fun f basDec = 
			   processBasDec(basDec, Env{loc=span, errStrms=errStrms, pts=[], preprocs=preprocs})
		       (* FIXME: check that the MLB file is valid *)
		       val _ = checkForErrors errStrms
		       val (errStrmss, ptss) = ListPair.unzip(List.map f basDecs)
		       in 
		           OS.FileSys.chDir dir;
			   (List.concat errStrmss, List.concat ptss)
		       end
		 | NONE => (checkForErrors errStrms; ([], []))
               (* end case *))
            end

  (* load a PML file *)
    and loadPML (filename, env as Env{loc, errStrms, pts, preprocs}) = let 
	val errStrm = Error.mkErrStream filename
	val (inStrm, reap) = preprocess(List.rev preprocs, filename)
        val ptOpt = Parser.parseFile (errStrm, inStrm)
        in
	   reap();
	   checkForErrors errStrms;
	   case ptOpt
            of NONE => NONE
	     | SOME pt => SOME (errStrm, pt)
        end

  (* load the MLB file *)
    fun load (errStrm, file) = let
	    val env0 = Env{loc=(0,0), errStrms=[], pts=[], preprocs=[]}
            in
	        loadMLB(file, env0)
            end

  end (* MLB *)
