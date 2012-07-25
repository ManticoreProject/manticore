(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FrontEnd (S: FRONT_END_STRUCTS): FRONT_END = 
struct

structure SourceMap : SOURCE_MAP = struct
  val map : AntlrStreamPos.sourcemap option ref = ref NONE
  fun getMap() = valOf (!map)
  fun setMap(m) = map := SOME(m)
end


open S

(* glue together the lexer and parser *)
structure Parser = MLParserFun (structure Lex = MLLexer
                                structure SourceMap = SourceMap
                                structure Ast = Ast)

fun posToReg (map, pos) = let
    val {fileName = file, lineNo=lineNo, colNo=colNo} = AntlrStreamPos.sourceLoc map pos
    val file = case file of SOME x => x | NONE => ""
    val sp = SourcePos.make({column=colNo, file=file, line=lineNo})
in
    Region.make({left=sp,
		 right=sp})
end

fun lexAndParse (file: File.t, ins: In.t): Ast.Program.t = let
    val source = Source.new file
    val sm = AntlrStreamPos.mkSourcemap()
    val lexer = MLLexer.lex sm {source=source}
    fun lexer'(a,b) = let
	val v = lexer(a,b)
	val (tok,_,_) = v
    in
	Out.outputl (Out.error, Tokens.toString tok);
	v
    end
    val _ = SourceMap.setMap(sm)
in
    case Parser.parse lexer (MLLexer.streamifyInstream ins)
     of (SOME pt, _, []) => (pt)
      | (_, _, errs) => (
        let
	    val _ = Out.outputl (Out.error, concat["FAILURE parsing file: ", file])
            val i = Source.lineStart source
	    fun parseError (pos, repair) = let
		fun toksToStr (toks) = String.concatWith((List.map (toks, Tokens.toString)), " ")
		val msg = (case repair
			    of AntlrRepair.Insert toks => ["syntax error; try inserting \"", toksToStr toks, "\""]
			     | AntlrRepair.Delete toks => ["syntax error; try deleting \"", toksToStr toks, "\""]
			     | AntlrRepair.Subst{old, new} => [
			       "syntax error; try substituting \"", toksToStr new, "\" for \"",
			       toksToStr old, "\""
			       ]
			     | AntlrRepair.FailureAt tok => ["syntax error at ", Tokens.toString tok]
			  (* end case *))
	    in
		Out.outputl (Out.error, String.concat ("ERR: "::msg));		
		Control.errorStr (posToReg(sm, pos), String.concat msg)
	    end
            val _ = List.map (errs, parseError)
        in
            Ast.Program.T []
        end)
end

fun lexAndParseFile (f: File.t) =
   File.withIn
   (f, fn ins => lexAndParse (f, ins))

val lexAndParseFile =
    Trace.trace ("FrontEnd.lexAndParseFile", File.layout, Ast.Program.layout)
    lexAndParseFile

end
