(* hlop-def-loader.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLOpDefLoader : sig

    type hlop_def = (HLOp.hlop * BOM.lambda)

    val load : Atom.atom -> hlop_def

  end = struct

    type hlop_def = (HLOp.hlop * BOM.lambda)

    structure Parser = HLOpDefParseFn(HLOpDefLex)

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	  val errToStr = Repair.repairToString HLOpDefTokens.toString srcMap
	  in
	    fn err => TextIO.print(concat["Error [", filename, "] ", errToStr err, "\n"])
	  end

  (* parse an input stream, returning a parse tree *)
    fun parse (filename, inStrm) = let
	  fun get () = TextIO.input inStrm
	  val srcMap = StreamPos.mkSourcemap()
	  val lexer = HLOpDefLex.lex srcMap
	  in
	    case Parser.parse lexer (HLOpDefLex.streamify get)
	     of (SOME pt, _, []) => SOME(Expand.cvtFile pt)
	      | (_, _, errs) => (
		  List.app (parseErr (filename, srcMap)) errs;
		  NONE)
	    (* end case *)
	  end

    structure Loader = LoaderFn(
      struct
	type file = HLOp.hlop * BOM.lambda
	val parse = parse
	val defaultSearchPath = Paths.hlopSearchPath
      end)

  (* a cache of previously loaded definitions *)
    val cache : hlop_def AtomTable.hash_table = AtomTbl.mkTable (128, Fail "HLOpDef table")

    fun load opName = (case AtomTable.find cache opName
	   of NONE => (case Loader.load(Atom.toString opName)
		 of SOME defn => (
		      AtomTable.insert cache (opName, defn);
		      defn)
		  | NONE => raise Fail("unable to load definition for @" ^ Atom.toString opName)
		(* end case *))
	    | SOME defn => defn
	  (* end case *))

  end
