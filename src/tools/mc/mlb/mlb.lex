(* mlb.lex
 *
 * Lexer for the MLB specification.
 *)


%name MLBLex;

%arg (lexErr);

%defs (

    structure T = MLBTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* the depth int ref will be used for keeping track of comment depth *)
    val depth = ref 0

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
);

%states INITIAL COMMENT;

%let letter = [a-zA-Z];
%let dig = [0-9];
%let idchar = {letter}|{dig}|"_"|"'"|"."|"-";
%let id = {letter}{idchar}*;
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let ws = " "|[\t\n\v\f\r];

<INITIAL> "="	=> (T.EQ);
<INITIAL> ";"	=> (T.SEMI);
<INITIAL> {id}	=> (idToken yytext);
<INITIAL> {ws}		=> (continue ());
<INITIAL> "(*"		=> (YYBEGIN COMMENT; depth := 1; continue());

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
