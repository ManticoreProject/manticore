structure MLBLex  = struct

    datatype yystart_state = 
STRING | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

 

    structure T = MLBTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* the depth int ref will be used for keeping track of comment depth *)
    val depth = ref 0

  (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

  (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

  (* make a string from buf *)
    fun mkString () = (T.STRING(String.concat(List.rev(!buf))) before buf := [])

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

  (* keyword lookup table *)
    local
      val find =
	  let val tbl = AtomTable.mkTable (17, Fail "keywords")
	      fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
	  in
	      app ins [
	        ("bas",         T.KW_bas),
	        ("basis",       T.KW_basis),
		("in",		T.KW_in),
		("let",		T.KW_let),
		("end",         T.KW_end),
		("and",         T.KW_and),
		("open",        T.KW_open),
		("local",       T.KW_local),
		("signature",   T.KW_signature),
		("functor",     T.KW_functor),
		("structure",   T.KW_structure)
	      ];
	      AtomTable.find tbl
	  end
    in
  (* return either a keyword token or a NAME token *)
    fun idToken id =
	let val ida = Atom.atom id
	in
	    case find ida
	     of NONE => T.NAME ida
	      | SOME kw => kw
	end
    end


      end

    local
    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
#[
]

    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yyarg as  lexErr)(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case UTF8.getu ULexBuffer.getc strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  T.EQ)
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SEMI)
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  idToken yytext
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  continue ())
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN COMMENT; depth := 1; continue())
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN STRING; continue())
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr(valOf(String.fromString yytext)); continue()
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  addStr yytext; continue()
      end
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; mkString())
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, [
				"bad escape character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue()
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr(yypos, [
				"bad character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue()
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         
	lexErr(yypos, ["bad character `", String.toString yytext, "'"]);
	continue()
      end
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
       
	depth := !depth + 1;
	continue())
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
       
	depth := !depth - 1;
        if (!depth = 0) then YYBEGIN INITIAL else ();
	continue ())
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  continue ())
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx28
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction2(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ25(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = yyAction0(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch : yymatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch : yymatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ26(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ19 (strm, lastMatch : yymatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch : yymatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ18(strm', lastMatch)
            else if inp < 0wx29
              then if inp = 0wx21
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx21
                  then if inp = 0wxE
                      then yyQ18(strm', lastMatch)
                    else if inp < 0wxE
                      then if inp <= 0wx8
                          then yyQ18(strm', lastMatch)
                          else yyQ19(strm', lastMatch)
                    else if inp = 0wx20
                      then yyQ19(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                else if inp = 0wx23
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx23
                  then yyQ20(strm', lastMatch)
                else if inp = 0wx28
                  then yyQ21(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = 0wx3E
              then yyQ18(strm', lastMatch)
            else if inp < 0wx3E
              then if inp = 0wx3C
                  then yyQ18(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx3B
                      then yyQ22(strm', lastMatch)
                      else yyQ18(strm', lastMatch)
                  else yyQ23(strm', lastMatch)
            else if inp = 0wx5B
              then yyQ18(strm', lastMatch)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyQ18(strm', lastMatch)
                  else yyQ24(strm', lastMatch)
            else if inp = 0wx61
              then yyQ24(strm', lastMatch)
            else if inp < 0wx61
              then yyQ18(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ24(strm', lastMatch)
              else yyQ18(strm', lastMatch)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ16(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ17(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx29
              then yyQ13(strm', lastMatch)
            else if inp < 0wx29
              then if inp = 0wx28
                  then yyQ14(strm', lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = 0wx2A
              then yyQ15(strm', lastMatch)
              else yyQ13(strm', lastMatch)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ11(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ11(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ10(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction9(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ10(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ7 (strm, lastMatch : yymatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx3A
                  then yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < 0wx3A
                  then if inp = 0wx23
                      then yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                    else if inp < 0wx23
                      then if inp = 0wx22
                          then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                          else yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                    else if inp <= 0wx2F
                      then yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyQ9(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp = 0wx5D
                  then yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < 0wx5D
                  then if inp = 0wx5C
                      then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp = 0wx61
                  then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < 0wx61
                  then yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp <= 0wx62
                  then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = 0wx73
              then yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx6F
                  then yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < 0wx6F
                  then if inp = 0wx6E
                      then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp = 0wx72
                  then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx74
                  then yyQ8(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyQ7(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx20
                  then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < 0wx20
                  then yyAction7(strm, yyNO_MATCH)
                else if inp = 0wx22
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = 0wx5D
              then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < 0wx23
              then if inp = 0wx20
                  then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < 0wx20
                  then yyAction7(strm, yyNO_MATCH)
                else if inp = 0wx22
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = 0wx5D
              then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= 0wx7E
              then yyQ12(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ4(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx20
                  then yyQ4(strm', lastMatch)
                else if inp < 0wx20
                  then yyQ3(strm', lastMatch)
                else if inp = 0wx22
                  then yyQ5(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ4(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyQ6(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp <= 0wx7E
              then yyQ4(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(yyarg as  lexErr)(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
yyarg(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
yyarg(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
