(* hlop-def-loader.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLOpDefLoader : sig

    type hlop_def = {
	inline : bool,			(* true, if the operations should always be inlined *)
					(* otherwise, a copy of the operator is added to the *)
					(* module. *)
	defn : BOM.lambda
      }

  (* an environment to keep track of any imports required by the high-level operator *)
    type import_env = BOM.var CFunctions.c_fun AtomTable.hash_table

    val load : (import_env * HLOp.hlop) -> hlop_def

  end = struct

    structure Parser = HLOpDefParseFn(HLOpDefLex)
    structure ATbl = AtomTable

    type hlop_def = {
	inline : bool,
	defn : BOM.lambda
      }

    type import_env = BOM.var CFunctions.c_fun ATbl.hash_table

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
    val cache : hlop_def ATbl.hash_table = ATbl.mkTable (128, Fail "HLOpDef table")

    fun load (importEnv, hlOp) = (case ATbl.find cache (HLOp.name hlOp)
	   of NONE => let
		val opName = HLOp.name hlOp
		in
		  case Loader.load(Atom.toString opName)
		   of SOME pt => let
			val defs = Expand.cvtFile(importEnv, pt)
			fun record (hlOp, inline, lambda) =
			      ATbl.insert cache (HLOp.name hlOp, {inline=inline, defn=lambda})
			in
			  List.app record defs;
			  ATbl.lookup cache opName
			end
		    | NONE => raise Fail("unable to load definition for @" ^ Atom.toString opName)
		  (* end case *)
		end
	    | SOME defn => defn
	  (* end case *))

  end
