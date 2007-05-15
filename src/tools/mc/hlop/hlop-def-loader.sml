(* hlop-def-loader.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLOpDefLoader : sig

    val load : Atom.atom -> (HLOp.hlop * BOM.lambda) option

  end = struct

    structure Parser = HLOpDefParseFn(HLOpDefLex)

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	  val errToStr = Repair.repairToString HLOpDefTokens.toString srcMap
	  in
	    fn err => TextIO.print(concat["Error [", filename, "] ", errToStr err, "\n"])
	  end

  (* parse a file, returning a parse tree *)
    fun parseFile filename = let
	  val file = TextIO.openIn filename
	  fun get () = TextIO.input file
	  val srcMap = StreamPos.mkSourcemap()
	  val lexer = HLOpDefLex.lex srcMap
	  in
	    case Parser.parse lexer (HLOpDefLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME pt)
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr (filename, srcMap)) errs;
		  NONE)
	    (* end case *)
	  end

    fun parse filename = (case parseFile filename
	   of SOME pt => Expand.cvtModule pt
	    | NONE => NONE
	  (* end case *))

    structure Loader = LoaderFn(
      struct
	type file = HLOp.hlop * BOM.lambda
	val 
  end
