(* parser.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parser glue.
 *)

structure Parser : sig

  (* parse a file; return NONE if there are syntax errors *)
    val parseFile : (Error.err_stream * string) -> ProgramParseTree.PML1.program option

  end = struct

  (* glue together the lexer and parser *)
    structure ManticoreParser = ManticoreParseFn(ManticoreLex)

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (* error function for parsers *)
    val parseErr = Error.parseError ManticoreTokens.toString

  (* parse a file, returning a parse tree *)
    fun parseFile (errStrm, filename) = let
	  val file = TextIO.openIn filename
	  fun get () = TextIO.input file
	  val lexer = ManticoreLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
	  in
	    case ManticoreParser.parse lexer (ManticoreLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME pt)
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr errStrm) errs;
		  NONE)
	    (* end case *)
	  end
       
    val parseFile = BasicControl.mkTracePassSimple {
	    passName = "parseFile",
	    pass = parseFile
	  }

  end
