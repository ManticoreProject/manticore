(* minmant.lex
 *
 * Based on CMSC 22610 Sample code (Winter 2006)
 * 
 * ml-ulex specification for MinML
 *)

%name MinMantLex;

%defs(

    structure T = MinMantTokens

  (* error function for lexers *)
    fun lexErr (lnum, msg) = Error.say [
	    "Error [", !Error.sourceFile, ":", Int.toString lnum, "]: ", msg, "\n"
	  ]

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
		("and",		T.KW_and),
		("andalso",	T.KW_andalso),
		("case",	T.KW_case),
		("datatype",	T.KW_datatype),
		("div",		T.KW_div),
		("else",	T.KW_else),
		("end",		T.KW_end),
		("fun",		T.KW_fun),
		("if",		T.KW_if),
		("in",		T.KW_in),
		("let",		T.KW_let),
		("mod",		T.KW_mod),
		("of",		T.KW_of),
		("orelse",	T.KW_orelse),
		("then",	T.KW_then),
		("type",	T.KW_type),
		("val",		T.KW_val)
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
%let idchar = {letter}|{dig}|"_"|"'";
%let id = {letter}{idchar}*;
%let tyvarid = "'"{idchar}*;
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let ws = " "|[\t\n\v\f\r];

<INITIAL> "("	=> (T.LP);
<INITIAL> ")"	=> (T.RP);
<INITIAL> "<="	=> (T.LTEQ);
<INITIAL> "<"	=> (T.LT);
<INITIAL> "::"	=> (T.DCOLON);
<INITIAL> "@"	=> (T.AT);
<INITIAL> "+"	=> (T.PLUS);
<INITIAL> "-"	=> (T.MINUS);
<INITIAL> "*"	=> (T.TIMES);
<INITIAL> "="	=> (T.EQ);
<INITIAL> "~"	=> (T.TILDE);
<INITIAL> ","	=> (T.COMMA);
<INITIAL> ";"	=> (T.SEMI);
<INITIAL> "|"	=> (T.BAR);
<INITIAL> "->"	=> (T.ARROW);
<INITIAL> "=>"	=> (T.DARROW);
<INITIAL> "_"	=> (T.WILD);
<INITIAL> {id}	=> (idToken yytext);
<INITIAL> {tyvarid}	=> (T.TYVAR(Atom.atom yytext));
<INITIAL> ({dig})+	=> (T.NUMBER(valOf (IntInf.fromString yytext)));
<INITIAL> {ws}		=> (continue ());
<INITIAL> "(*"		=> (YYBEGIN COMMENT; depth := 1; continue());
<INITIAL> "\""		=> (YYBEGIN STRING; continue());

<STRING>{esc}		=> (addStr(valOf(String.fromString yytext)); continue());
<STRING>{sgood}+	=> (addStr yytext; continue());
<STRING>"\""		=> (YYBEGIN INITIAL; mkString());
<STRING>"\\".		=> (lexErr(!yylineno, concat[
				"bad escape character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue());
<STRING>.		=> (lexErr(!yylineno, concat[
				"bad character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue());

<INITIAL> . => (
	lexErr(!yylineno, concat["bad character `", String.toString yytext, "'"]);
	continue());

<COMMENT> "(*" => (
	depth := !depth + 1;
	continue());
<COMMENT> "*)" => (
	depth := !depth - 1;
        if (!depth = 0) then YYBEGIN INITIAL else ();
	continue ());
<COMMENT> .|"\n" => (continue ());
