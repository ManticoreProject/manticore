(* hlop-def-loader.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure HLOpDefLoader : sig

    type hlop_def = {
	inline : bool,		(* true, if the operations should always be inlined *)
				(* otherwise, a copy of the operator is added to the *)
				(* module. *)
	defn : BOM.lambda,
	cfuns : (BOM.var * int) list
				(* list of C functions that the definition references and *)
				(* theur use counts in the definition. *)
      }

  (* an environment to keep track of any imports required by the high-level operator *)
    type import_env = BOM.var CFunctions.c_fun AtomTable.hash_table

    val loadPrototypes : unit -> unit
    val load : (import_env * HLOp.hlop) -> hlop_def

  end = struct

    structure Parser = HLOpDefParseFn(HLOpDefLex)
    structure ATbl = AtomTable

    type hlop_def = {
	inline : bool,
	defn : BOM.lambda,
	cfuns : (BOM.var * int) list
      }

    type import_env = BOM.var CFunctions.c_fun ATbl.hash_table

    fun tokToString (HLOpDefTokens.STRING s) = concat["\"", String.toString s, "\""]
      | tokToString (HLOpDefTokens.FLOAT flt) = FloatLit.toString flt
      | tokToString (HLOpDefTokens.POSINT n) = IntInf.toString n
      | tokToString (HLOpDefTokens.NEGINT n) = "-" ^ IntInf.toString(~n)
      | tokToString (HLOpDefTokens.HLOP id) = "@" ^ Atom.toString id
      | tokToString (HLOpDefTokens.ID id) = Atom.toString id
      | tokToString tok = HLOpDefTokens.toString tok

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	  val errToStr = AntlrRepair.repairToString tokToString srcMap
	  in
	    fn err => TextIO.print(concat["Error ", errToStr err, "\n"])
	  end

  (* parse an input stream, returning a parse tree *)
    fun parse (filename, inStrm) = let
	  fun get () = TextIO.input inStrm
	  val srcMap = AntlrStreamPos.mkSourcemap()
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

   fun loadPrototypes () = let
       val prototypeFile = "prototypes.hlop"
       in
         case Loader.load prototypeFile
	  of SOME pt => let
              val defs = Expand.cvtPrototypes {fileName=prototypeFile, pt=pt}
	      in 
		 List.app HLOpEnv.define defs
	      end
	 (* end case *)
       end (* loadPrototypes *)

  (* a cache of previously loaded definitions *)
    val cache : hlop_def ATbl.hash_table = ATbl.mkTable (128, Fail "HLOpDef table")

    fun load (importEnv, hlOp) = (case ATbl.find cache (HLOp.name hlOp)
	   of NONE => let
		val opName = HLOp.name hlOp
		val fileName = OS.Path.joinBaseExt{base=Atom.toString opName, ext=SOME "hlop"}
		in
		  case Loader.load fileName
		   of SOME pt => let
			val defs = Expand.cvtFile(importEnv, fileName, pt)
			fun record {name, inline, def, externs} = (
			    (* compute census info for the definition; we have to decrement the
			     * imported C function counts, because they get counted when a
			     * definition is added to the program.
			     *)
			      Census.initLambda def;
			      List.app (fn (cf, cnt) => BOM.Var.addToCount(cf, ~cnt)) externs;
			      ATbl.insert cache
				(HLOp.name hlOp, {inline=inline, defn=def, cfuns=externs}))
			in
			  List.app record defs;
			  case ATbl.find cache opName
			   of NONE => raise Fail(concat[
				  fileName,
				  " does not contain definition of @",
				  Atom.toString opName
				])
			    | SOME defn => defn
			end
		    | NONE => raise Fail("unable to load definition for @" ^ Atom.toString opName)
		  (* end case *)
		end
	    | SOME defn => defn
	  (* end case *))

  end
