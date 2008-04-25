(* parser.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Based on CMSC 22610 Sample code (Winter 2007)
 *)

structure Parser : sig

  (* parse a file; return NONE if there are syntax errors *)
    val parseFile : (Error.err_stream * string) -> ParseTree.program option

  end = struct

  (* glue together the lexer and parser *)
    structure MantParser = MantParseFn(MantLex)

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (* error function for parsers *)
    val parseErr = Error.parseError MantTokens.toString

  (* parse a file, returning a parse tree *)
    fun parseFile (errStrm, filename) = let
	  val file = TextIO.openIn filename
	  fun get () = TextIO.input file
	  val lexer = MantLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
	  in
	    case MantParser.parse lexer (MantLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME pt)
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr errStrm) errs;
		  NONE)
	    (* end case *)
	  end
       
    val parseFile =
       BasicControl.mkTracePassSimple
       {passName = "parseFile",
        pass = parseFile}

  end
