(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MLBFrontEnd (S: MLB_FRONT_END_STRUCTS): MLB_FRONT_END =
struct

structure Option = MLtonOption
structure List = MLtonList
fun String_equals (s1 : string, s2) = (s1 = s2)
fun String_fromListRev l = String.implode(List.rev l)
fun String_hash s = CharVector.foldl (fn (c, h) => Word.fromInt(Char.ord c) + Word.* (h, 0w31)) 0w0 s
fun String_concatWith (l, s) = String.concatWith s l

structure SourceMap : SOURCE_MAP =
  struct
    val map : AntlrStreamPos.sourcemap option ref = ref NONE
    fun getMap() = valOf (!map)
    fun setMap(m) = map := SOME(m)
  end

open S

(* The lexer recursively invokes the lexer/parser when it encounters a file
 * reference.  So, we need a stub here to feed to the lexer.  The stub is
 * overridden after the lexer is defined.
 *)

val lexAndParseProgOrMLBRef: (File.t * Region.t -> Ast.Basdec.node) ref =
   ref (fn _ => Error.bug "MLBFrontEnd.lexAndParseProgOrMLB")

val lexAndParseProgOrMLB = fn f => !lexAndParseProgOrMLBRef f

(* glue together the lexer and parser *)
structure Parser = MLBParserFun (
	structure Lex = MLBLexer
	structure SourceMap = SourceMap
	structure Ast = Ast
	val lexAndParseProgOrMLB = lexAndParseProgOrMLB)

fun posToReg (map, pos) = let
      val {fileName = file, lineNo=lineNo, colNo=colNo} = AntlrStreamPos.sourceLoc map pos
      val file = case file of SOME x => x | NONE => ""
      val sp = SourcePos.make {column=colNo, file=file, line=lineNo}
      in
	Region.make {left=sp, right=sp}
      end

fun lexAndParse (source: Source.t, ins: In.t) = let
      val sm = AntlrStreamPos.mkSourcemap()
      val lexer = MLBLexer.lex sm {source=source}
      val _ = SourceMap.setMap sm
(* DEBUG *)      val _ = print "starting lexAndParse\n"
      in
	case Parser.parse lexer (MLBLexer.streamifyInstream ins)
	 of (SOME pt, _, []) => (Ast.Basdec.checkSyntax pt; pt)
	  | (_, _, errs) => let
	      val _ = Out.outputl (Out.error, concat["FAILURE parsing file: ", Source.file source])
	      val i = Source.lineStart source
	      fun parseError (pos, repair) = let
		    fun toksToStr (toks) = (*String.concatWith*)String_concatWith((MLtonList.map (toks, MLBTokens.toString)), " ")
		    val msg = (case repair
			    of AntlrRepair.Insert toks => ["syntax error; try inserting \"", toksToStr toks, "\""]
			     | AntlrRepair.Delete toks => ["syntax error; try deleting \"", toksToStr toks, "\""]
			     | AntlrRepair.Subst{old, new} => [
			       "syntax error; try substituting \"", toksToStr new, "\" for \"",
			       toksToStr old, "\""
			       ]
			     | AntlrRepair.FailureAt tok => ["syntax error at ", MLBTokens.toString tok]
			  (* end case *))
		    in
		      Out.outputl (Out.error, String.concat ("ERR: "::msg));
		      Control.errorStr (posToReg(sm, pos), String.concat msg)
		    end
	      val _ = MLtonList.map (errs, parseError)
	      in
		Ast.Basdec.empty
	      end
	(* end case *)
      end

fun lexAndParseFile (f: File.t) = File.withIn (f, fn ins => lexAndParse (Source.new f, ins))

val lexAndParseFile =
      Trace.trace ("MLBFrontEnd.lexAndParseFile", File.layout, Ast.Basdec.layout)
        lexAndParseFile

fun lexAndParseString (s: (*String.t*)string) = let
      val source = Source.new "<string>"
      val ins = In.openString s
      in
	lexAndParse (source, ins)
      end

val lexAndParseString =
      Trace.trace
	("MLBFrontEnd.lexAndParseString", Layout.str, Ast.Basdec.layout)
	  lexAndParseString

val lexAndParseString =
   fn (s: string) =>
   let
      val cwd = Dir.current ()
      val relativize = SOME cwd
      val state = {cwd = cwd, relativize = relativize, seen = []}
      val psi : (File.t * Ast.Basdec.t Promise.t) HashSet.t =
         HashSet.new {hash = (*String.hash*)String_hash o #1}
      local
         val pathMap =
             Control.mlbPathMap ()
         fun peekPathMap var' =
            case List.peek (pathMap, fn {var,...} =>
                            var = var') of
               NONE => NONE
             | SOME {path, ...} => SOME path
      in
         val peekPathMap =
            Trace.trace ("MLBFrontEnd.peekPathMap",
                         (*String.layout*)Layout.str,
                         Option.layout Dir.layout)
            peekPathMap
      end
      fun expandPathVars (path, seen, region) =
         let
            fun loop (s, acc, accs) =
               case s of
                  [] => String.concat (List.rev
                                       ((*String.fromListRev*)String_fromListRev acc :: accs))
                | #"$" :: #"(" :: s =>
                     let
                        val accs = (*String.fromListRev*)String_fromListRev acc :: accs
                        fun loopVar (s, acc) =
                           case s of
                              [] => Error.bug "MLBFrontEnd.lexAndParseString.expandPathVars"
                            | #")" :: s => (s, (*String.fromListRev*)String_fromListRev acc)
                            | c :: s => loopVar (s, c :: acc)
                        val (s, var) = loopVar (s, [])
                     in
                        if List.exists (seen, fn x => x = var)
                           then
                              let
                                 open Layout
                              in
                                 Control.error
                                 (region,
                                  str "Cyclic MLB path variables",
                                  List.layout Layout.str (var :: seen))
                                 ; loop (s, [], accs)
                              end
                        else
                           case peekPathMap var of
                              NONE =>
                                 let
                                    open Layout
                                 in
                                    Control.error
                                    (region,
                                     seq [str "Undefined MLB path variable: ",
                                          str var],
                                     empty)
                                    ; loop (s, [], accs)
                                 end
                            | SOME path =>
                                 loop (s, [],
                                       expandPathVars (path, var :: seen, region)
                                       :: accs)
                     end
                | c :: s => loop (s, c :: acc, accs)
         in
            loop (String.explode path, [], [])
         end
      fun regularize {fileOrig, cwd, region, relativize} =
         let
            val fileExp = expandPathVars (fileOrig, [], region)
            val fileAbs = OS.Path.mkAbsolute {path = fileExp, relativeTo = cwd}
            val fileAbs = OS.Path.mkCanonical fileAbs
            val relativize =
               if !Control.preferAbsPaths orelse OS.Path.isAbsolute fileExp
                  then NONE
                  else relativize
            val fileUse =
               case relativize of
                  NONE => fileAbs
                | SOME d => OS.Path.mkRelative {path = fileAbs, relativeTo = d}
         in
            {fileAbs = fileAbs,
             fileUse = fileUse,
             relativize = relativize}
         end
      val regularize =
         Trace.trace ("MLBFrontEnd.lexAndParseString.regularize",
                      fn {fileOrig, cwd, relativize, ...} =>
                      Layout.record
                      [("fileOrig", File.layout fileOrig),
                       ("cwd", Dir.layout cwd),
                       ("relativize", Option.layout Dir.layout relativize)],
                      fn {fileAbs, fileUse, relativize} =>
                      Layout.record
                      [("fileAbs", File.layout fileAbs),
                       ("fileUse", File.layout fileUse),
                       ("relativize", Option.layout Dir.layout relativize)])
         regularize
      fun lexAndParseProg {fileAbs: File.t, fileOrig: File.t, fileUse: File.t,
                           fail: (*String.t*)string -> Ast.Program.t} =
         Ast.Basdec.Prog
         ({fileAbs = fileAbs, fileUse = fileUse},
          Promise.delay
          (fn () =>
           Control.checkFile
           (fileUse, {fail = fail,
                      name = fileOrig,
                      ok = fn () => FrontEnd.lexAndParseFile fileUse})))
      and lexAndParseMLB {relativize: Dir.t option,
                          seen: (File.t * File.t * Region.t) list,
                          fileAbs: File.t, fileOrig: File.t, fileUse: File.t,
                          fail: (*String.t*)string -> Ast.Basdec.t, reg: Region.t} =
         Ast.Basdec.MLB
         ({fileAbs = fileAbs, fileUse = fileUse},
          Promise.delay
          (fn () =>
           Control.checkFile
           (fileUse,
            {fail = fail,
             name = fileOrig,
             ok = fn () => let
                val seen' = (fileAbs, fileUse, reg) :: seen
             in
                if List.exists (seen, fn (fileAbs', _, _) =>
                                (*String.equals*)String_equals (fileAbs, fileAbs'))
                   then (let open Layout
                   in
                            Control.error
                            (reg, seq [str "Basis forms a cycle with ",
                                       File.layout fileUse],
                             align (List.map (seen', fn (_, f, r) =>
                                              seq [Region.layout r,
                                                   str ": ",
                                                   File.layout f])))
                            ; Ast.Basdec.empty
                   end)
                else
                   let
                      val (_, basdec) =
                         HashSet.lookupOrInsert
                         (psi, (*String.hash*)String_hash fileAbs, fn (fileAbs', _) =>
                          (*String.equals*)String_equals (fileAbs, fileAbs'), fn () =>
                          let
                             val cwd = OS.Path.dir fileAbs
                             val basdec =
                                Promise.delay
                                (fn () =>
                                 wrapLexAndParse
                                 ({cwd = cwd,
                                   relativize = relativize,
                                   seen = seen'},
                                  lexAndParseFile, fileUse))
                          in
                             (fileAbs, basdec)
                          end)
                   in
                      Promise.force basdec
                   end
             end})))
      and lexAndParseProgOrMLB {cwd, relativize, seen}
                               (fileOrig: File.t, reg: Region.t) =
         Exn.withEscape
         (fn escape =>
          let
             fun fail default msg =
                let
                   val () = Control.error (reg, Layout.str msg, Layout.empty)
                in
                   default
                end
             fun err msg =
                fail (Ast.Basdec.Seq []) (concat ["File ", fileOrig, msg])
             val {fileAbs, fileUse, relativize, ...} =
                regularize {cwd = cwd,
                            fileOrig = fileOrig,
                            region = reg,
                            relativize = relativize}
                handle _ => escape (err " could not be regularized")
             val mlbExts = ["mlb"]
             val progExts = ["ML","fun","sig","sml","pml"]
             fun errUnknownExt () = err " has an unknown extension"
          in
             case File.extension fileUse of
                NONE => errUnknownExt ()
              | SOME s =>
                   if List.contains (mlbExts, s, (*String.equals*)String_equals) then
                      lexAndParseMLB {relativize = relativize,
                                      seen = seen,
                                      fileAbs = fileAbs,
                                      fileOrig = fileOrig,
                                      fileUse = fileUse,
                                      fail = fail Ast.Basdec.empty,
                                      reg = reg}
                   else if List.contains (progExts, s, (*String.equals*)String_equals) then
                      lexAndParseProg {fileAbs = fileAbs,
                                       fileOrig = fileOrig,
                                       fileUse = fileUse,
                                       fail = fail Ast.Program.empty}
                   else errUnknownExt ()
          end)
      and wrapLexAndParse (state, lexAndParse, arg) =
         Ref.fluidLet
         (lexAndParseProgOrMLBRef, lexAndParseProgOrMLB state, fn () =>
          lexAndParse arg)
      val dec = wrapLexAndParse (state, lexAndParseString, s)
   in
      dec
   end

end
