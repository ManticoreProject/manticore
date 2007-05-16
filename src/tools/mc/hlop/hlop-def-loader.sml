(* hlop-def-loader.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLOpDefLoader : sig

  (* an environment to keep track of any imports required by the high-level operator *)
    type import_env = BOM.var CFunctions.c_function AtomTable.hash_table

    val load : (import_env * HLOp.hlop) -> {
	    inline : bool,
	    defn : BOM.lambda
	  }

  end = struct

    structure Parser = HLOpDefParseFn(HLOpDefLex)
    structure ATbl = AtomTable

    type import_env = BOM.var CFunctions.c_function ATbl.hash_table

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
	     of (SOME pt, _, []) => SOME pt
	      | (_, _, errs) => (
		  List.app (parseErr (filename, srcMap)) errs;
		  NONE)
	    (* end case *)
	  end

    structure Loader = LoaderFn(
      struct
	type file = HLOpDefPT.file
	val parse = parse
	val defaultSearchPath = Paths.hlopSearchPath
      end)

  (* a cache of previously loaded definitions *)
    val cache : BOM.lambda ATbl.hash_table = ATbl.mkTable (128, Fail "HLOpDef table")

    fun load hlOp = (case ATbl.find cache (HLOp.name hlOp)
	   of NONE => let
		val opName = HLOp.name hlOp
		in
		  case Loader.load(Atom.toString opName)
		   of SOME pt =>
(* FIXME: convert pt; insert defns, etc. *)raise Fail "load"
		    | NONE => raise Fail("unable to load definition for @" ^ Atom.toString opName)
		  (* end case *)
		end
	    | SOME defn => defn
	  (* end case *))

  end
