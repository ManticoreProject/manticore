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

    fun revConcat ls = List.concat (List.rev ls)
    fun concatMap f ls = List.concat (List.map f ls)

    fun preprocess (ppCmd :: args, file) = let
	    val proc = Unix.execute(ppCmd, args@[file])
            in 
	       { inStrm = Unix.textInstreamOf proc,
		 reap = fn () => ignore(Unix.reap proc) 
	       }
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

    datatype mlb_env
      = Env of 
             {
		loc : Error.span, 
		errStrms : Error.err_stream list, 
		pts : PPT.program list,
		dir : string
              }

  (* process a basis expression *)
    fun processBasExp (mlb, env as Env{loc, errStrms, pts, dir}) = (case mlb
        of PT.MarkBasExp {span, tree} => 
	     processBasExp (tree, Env{loc=span, errStrms=errStrms, pts=pts, dir=dir})
	 | PT.DecBasExp basDec => 
	     processBasDec (basDec, env)
	 | _ => raise Fail "todo"
        (* end case *))

  (* process a basis declaration *)
    and processBasDec (basDec, env as Env{loc, errStrms, pts, dir}) = (case basDec
        of PT.MarkBasDec {span, tree} => 
	   processBasDec (tree, Env{loc=span, errStrms=errStrms, pts=pts, dir=dir})
	 (* imports can be pml or mlb files *)
	 | PT.ImportBasDec (file, preprocessor) => let
           val file = Atom.toString file
	   val errStrms = Error.mkErrStream file :: errStrms
           in
	      case OS.Path.splitBaseExt file
	       of {base, ext=SOME "mlb"} => let
		      val (errStrms', pts') = loadMLB (file, preprocessor, Env{loc=loc, errStrms=errStrms, pts=pts, dir=dir})		      
		      in
		          (errStrms'@errStrms, pts'@pts)
		      end
		| {base, ext=SOME "pml"} => (
                  case loadPML (file, preprocessor, Env{loc=loc, errStrms=errStrms, pts=pts, dir=dir})
		   of NONE => (errStrms, pts)
		    | SOME (errStrm, pt) => (errStrm :: errStrms, pt :: pts)
                  (* end case *))
		| _ => raise Fail "unknown source file extension"
           end
	 | _ => raise Fail "todo"
        (* end case *))

  (* load an MLB file *)
    and loadMLB (file, preprocessor, env as Env{loc, errStrms, pts, dir}) = let
	  val errStrm = Error.mkErrStream file
          in
	      (case MLBParser.parseFile (errStrm, file)
		of SOME {span, tree=basDecs} => let
		       val {dir=dir', ...} = OS.Path.splitDirFile file
		       val dir' = OS.FileSys.fullPath dir'
		       val _ = OS.FileSys.chDir dir'
		       fun f basDec = 
			   processBasDec(basDec, Env{loc=span, errStrms=errStrms, pts=[], dir=dir})
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
    and loadPML (filename, preprocessor, env as Env{loc, errStrms, pts, dir}) = let 
	val errStrm = Error.mkErrStream filename
	val { inStrm, reap } = (case preprocessor
		     of NONE => { inStrm = TextIO.openIn filename, 
				  reap = fn () => () 
				}
		      | SOME ppCmd => preprocess (ppCmd, filename)
		   (* end case *))
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
	    val dir = OS.FileSys.fullPath "."
	    val env0 = Env{loc=(0,0), errStrms=[], pts=[], dir=dir}
            in
	        loadMLB(file, NONE, env0)
            end

  end (* MLB *)
