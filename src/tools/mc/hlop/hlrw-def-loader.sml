(* hlrw-def-loader.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Loader structure for high-level rewrites.
 *)

structure HLRWDefLoader : sig

    val loadFromModuleName : Atom.atom -> HLRWDefPT.file

    val load : HLOp.hlop -> HLRWDefPT.file list

end = struct

    structure Parser = HLRWDefParseFn(HLRWDefLex)
    structure ATbl = AtomTable

    fun tokToString (HLRWDefTokens.NUM n) = IntInf.toString n
      | tokToString (HLRWDefTokens.HLOP id) = "@" ^ Atom.toString id
      | tokToString (HLRWDefTokens.ID id) = Atom.toString id
      | tokToString tok = HLRWDefTokens.toString tok

    (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	val errToStr = AntlrRepair.repairToString tokToString srcMap
        fun fmtErr err = TextIO.print(concat["Error ", errToStr err, "\n"])
    in
        fmtErr
    end

    (* parse an input stream, returning a parse tree *)
    fun parse (filename, inStrm) = let
        fun get () = TextIO.input inStrm
        val srcMap = AntlrStreamPos.mkSourcemap()
        val lexer = HLRWDefLex.lex srcMap
    in
        case Parser.parse lexer (HLRWDefLex.streamify get)
         of (SOME pt, _, []) => SOME pt
          | (_, _, errs) => (List.app (parseErr (filename, srcMap)) errs;
                             NONE)
	(* end case *)
    end (* parse *)

    structure RWLoader = LoaderFn(
        struct
            type file = HLRWDefPT.file
            val parse = parse
            val defaultSearchPath = Paths.hlopSearchPath
        end)

    (* a cache of previously loaded definitions *)
    val cache : HLRWDefPT.file ATbl.hash_table =
        ATbl.mkTable (32, Fail "HLRWDef table")

    fun loadFromModuleName moduleName = (case ATbl.find cache moduleName
         of NONE => let
                val fileName = OS.Path.joinBaseExt {base=Atom.toString
                                                        moduleName,
                                                    ext=SOME "hlrw"}
            in case RWLoader.load fileName
                of SOME pt => (ATbl.insert cache (moduleName, pt); pt)
                 | NONE => raise Fail("unable to load rewrite module " ^
                                      (Atom.toString moduleName))
            end
          | SOME defn => defn
        (* end case *))

    fun load hlOp = let
        val rwModules = HLOpDefLoader.loadRewrites hlOp
    in
        List.map loadFromModuleName rwModules
    end (* load *)

end (* struct *)
