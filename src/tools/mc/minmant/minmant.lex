(* minmant.lex
 *
 * Based on CMSC 22610 Sample code (Winter 2006)
 * 
 * ml-ulex specification for MinML
 *)

%name MinMantLex;

%arg (lexErr);

%defs(

    structure T = MinMantTokens

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

  (* make a FLOAT token from a substring *)
    fun mkFloat ss = let
	  val (isNeg, rest) = (case Substring.getc ss
		 of SOME(#"-", r) => (true, r)
		  | SOME(#"~", r) => (true, r)
		  | _ => (false, ss)
		(* end case *))
	  val (whole, rest) = Substring.splitl Char.isDigit rest
	  val rest = Substring.triml 1 rest (* remove "." *)
	  val (frac, rest) = Substring.splitl Char.isDigit rest
	  val exp = if Substring.isEmpty rest
		then 0
		else let
		  val rest = Substring.triml 1 rest (* remove "e" or "E" *)
		  in
		    #1(valOf(Int.scan StringCvt.DEC Substring.getc rest))
		  end
	  in
	    T.FLOAT(FloatLit.float{
		isNeg = isNeg,
		whole = Substring.string whole,
		frac = Substring.string frac,
		exp = exp
	      })
	  end

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
		("and",		T.KW_and),
		("andalso",	T.KW_andalso),
		("by",          T.KW_by),
		("case",	T.KW_case),
		("datatype",	T.KW_datatype),
		("div",		T.KW_div),
		("else",	T.KW_else),
		("end",		T.KW_end),
		("exception",	T.KW_exception),
		("fn",          T.KW_fn),
		("fun",		T.KW_fun),
		("handle",	T.KW_handle),
		("if",		T.KW_if),
		("in",		T.KW_in),
		("let",		T.KW_let),
		("local",	T.KW_let),
		("mod",		T.KW_mod),
		("of",		T.KW_of),
		("orelse",	T.KW_orelse),
		("pval",	T.KW_pval),
		("dval",	T.KW_dval),	(* temporaray *)
		("raise",	T.KW_raise),
		("spawn",       T.KW_spawn),
		("then",	T.KW_then),
		("to",          T.KW_to),
		("type",	T.KW_type),
		("val",		T.KW_val),
		("where",       T.KW_where)
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
);

%states INITIAL STRING COMMENT;

%let letter = [a-zA-Z];
%let dig = [0-9];
%let num = {dig}+;
%let idchar = {letter}|{dig}|"_"|"'";
%let id = {letter}{idchar}*;
%let tyvarid = "'"{idchar}*;
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let ws = " "|[\t\n\v\f\r];

<INITIAL> "("	=> (T.LP);
<INITIAL> ")"	=> (T.RP);
<INITIAL> "["	=> (T.LB);
<INITIAL> "]"	=> (T.RB);
<INITIAL> "(|"  => (T.LPB);
<INITIAL> "|)"  => (T.RPB);
<INITIAL> "[|"  => (T.LBB);
<INITIAL> "|]"  => (T.RBB);
<INITIAL> "<="	=> (T.LTEQ);
<INITIAL> "<"	=> (T.LT);
<INITIAL> "<>"  => (T.NEQ);
<INITIAL> ">"   => (T.GT);
<INITIAL> ">="  => (T.GTEQ);
<INITIAL> "::"	=> (T.DCOLON);
<INITIAL> "@"	=> (T.AT);
<INITIAL> "^"	=> (T.CONCAT);
<INITIAL> "!"   => (T.PSUB);
<INITIAL> "+"	=> (T.PLUS);
<INITIAL> "-"	=> (T.MINUS);
<INITIAL> "*"	=> (T.TIMES);
<INITIAL> "/"	=> (T.SLASH);
<INITIAL> "="	=> (T.EQ);
<INITIAL> "~"	=> (T.TILDE);
<INITIAL> ","	=> (T.COMMA);
<INITIAL> ";"	=> (T.SEMI);
<INITIAL> "|"	=> (T.BAR);
<INITIAL> ":"	=> (T.COLON);
<INITIAL> "->"	=> (T.ARROW);
<INITIAL> "=>"	=> (T.DARROW);
<INITIAL> "_"	=> (T.WILD);
<INITIAL> "|?|" => (T.PCHOICE);
<INITIAL> {id}	=> (idToken yytext);
<INITIAL> {tyvarid}	=> (T.TYVAR(Atom.atom yytext));
<INITIAL> "~"?{num}	=> (T.INT(valOf (IntInf.fromString yytext)));
<INITIAL> "~"?{num}"."{num}([eE][+~]?{num})?
			=> (mkFloat yysubstr);
<INITIAL> {ws}		=> (continue ());
<INITIAL> "(*"		=> (YYBEGIN COMMENT; depth := 1; continue());
<INITIAL> "\""		=> (YYBEGIN STRING; continue());

<STRING>{esc}		=> (addStr(valOf(String.fromString yytext)); continue());
<STRING>{sgood}+	=> (addStr yytext; continue());
<STRING>"\""		=> (YYBEGIN INITIAL; mkString());
<STRING>"\\".		=> (lexErr(yypos, [
				"bad escape character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue());
<STRING>.		=> (lexErr(yypos, [
				"bad character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue());

<INITIAL> . => (
	lexErr(yypos, ["bad character `", String.toString yytext, "'"]);
	continue());

<COMMENT> "(*" => (
	depth := !depth + 1;
	continue());
<COMMENT> "*)" => (
	depth := !depth - 1;
        if (!depth = 0) then YYBEGIN INITIAL else ();
	continue ());
<COMMENT> .|"\n" => (continue ());
