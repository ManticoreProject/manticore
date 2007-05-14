(* bom-parser.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse and expand a BOM term from a file
 *)

structure BOMParser : sig

    val parseFile : string -> BOMPT.module option
    val parse : string -> BOM.module

  end = struct

    structure Parser = BOMParseFn(BOMLex)

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	  val errToStr = Repair.repairToString BOMTokens.toString srcMap
	  in
	    fn err => TextIO.print(concat["Error [", filename, "] ", errToStr err, "\n"])
	  end

  (* parse a file, returning a parse tree *)
    fun parseFile filename = let
	  val file = TextIO.openIn filename
	  fun get () = TextIO.input file
	  val srcMap = StreamPos.mkSourcemap()
	  val lexer = BOMLex.lex srcMap
	  in
	    case Parser.parse lexer (BOMLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME pt)
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr (filename, srcMap)) errs;
		  NONE)
	    (* end case *)
	  end

    fun parse filename = (case parseFile filename
	   of SOME pt => Expand.cvtModule pt
	    | NONE => raise Fail "bad input"
	  (* end case *))

  end
