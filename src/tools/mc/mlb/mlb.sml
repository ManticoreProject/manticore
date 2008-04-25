(* mlb.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Implementation of MLB.
 *)

structure MLB : sig
    
  (* load the compilation unit of an MLB file *)
    val load : (Error.err_stream * string) -> (Error.err_stream * ParseTree.program) list

  end = struct

    structure PT = MLBParseTree

    exception Error

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
    fun processBasExp (loc, errStrms, mlb, ptss) = (case mlb
        of PT.MarkBasExp {span, tree} => processBasExp (span, errStrms, tree, ptss)
	 | PT.DecBasExp basDec => processBasDec (loc, errStrms, basDec, ptss)
	 | _ => raise Fail "todo"
        (* end case *))

  (* process a basis declaration *)
    and processBasDec (loc, errStrms, basDec, ptss) = (case basDec
        of PT.MarkBasDec {span, tree} => processBasDec (span, errStrms, tree, ptss)
	 (* imports can be pml or mlb files *)
	 | PT.ImportBasDec file => let
           val file = Atom.toString file
	   val errStrms = Error.mkErrStream file :: errStrms
           in
	      case OS.Path.splitBaseExt file
	       of {base, ext=SOME "mlb"} => loadMLB (errStrms, file, ptss)
		| {base, ext=SOME "pml"} => loadPML (errStrms, file, ptss)
		| _ => raise Fail "unknown source file extension"
           end
	 | _ => raise Fail "todo"
        (* end case *))

  (* load an MLB file *)
    and loadMLB (errStrms, file, ptss) = (case MLBParser.parseFile (List.hd errStrms, file)
        of SOME {span, tree=basDecs} => let
 	   fun f (basDec, ptss) = processBasDec (span, errStrms, basDec, ptss)
	   (* FIXME: check that the MLB file is valid *)
           in 
	       checkForErrors errStrms;
               List.foldl f ptss basDecs
           end
	 | NONE => (checkForErrors errStrms; [])
       (* end case *))

  (* load a PML file *)
    and loadPML (errStrms, file, pts) = let 
	val errStrm = List.hd errStrms
        val ptOpt = FrontEnd.load'(errStrm, file)
        in
	   checkForErrors errStrms;
	   case ptOpt
            of NONE => pts
	     | SOME pt => [(errStrm, pt)] :: pts
        end

  (* load the MLB file *)
    fun load (errStrm, file) = List.concat (List.rev (loadMLB([errStrm], file, [])))

  end (* MLB *)
