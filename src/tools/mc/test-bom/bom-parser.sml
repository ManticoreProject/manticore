(* bom-parser.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse and expand a BOM term from a file
 *)

structure BOMParser : sig

    val parse : (Error.err_stream * string) -> BOM.module option

  end = struct

    structure Parser = BOMParseFn(BOMLex)

  (* error function for parsers *)
    val parseErr = Error.parseError BOMTokens.toString

  (* parse a file, returning a parse tree *)
    fun parse (errStrm, filename) = let
	  val file = TextIO.openIn filename
	  fun get () = TextIO.input file
	  val lexer = BOMLex.lex (Error.sourceMap errStrm)
	  in
	    case Parser.parse lexer (BOMLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME(Expand.cvtModule pt))
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr errStrm) errs;
		  NONE)
	    (* end case *)
	  end

  end
