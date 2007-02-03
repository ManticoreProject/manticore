(* cps-parser.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parse and expand a CPS term from a file
 *)

structure CPSParser : sig

    val parse : string -> CPS.module

  end = struct

    structure Parser = CPSParseFn(CPSLex)

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	  val errToStr = Repair.repairToString CPSTokens.toString srcMap
	  in
	    fn err => TextIO.print(concat["Error [", filename, "] ", errToStr err, "\n"])
	  end

  (* parse a file, returning a parse tree *)
    fun parseFile filename = let
	  val file = TextIO.openIn filename
	  fun get () = TextIO.input file
	  val srcMap = StreamPos.mkSourcemap()
	  val lexer = CPSLex.lex srcMap
	  in
	    case Parser.parse lexer (CPSLex.streamify get)
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
