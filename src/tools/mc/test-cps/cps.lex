(* cps.lex
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

%name CPSLex;

%defs(

    structure T = CPSParseToks

  (* error function for lexers *)
    fun lexErr (lnum, msg) = TextIO.print(concat [
	    "Error [line ", Int.toString lnum, "]: ", msg, "\n"
	  ])

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

  (* the depth int ref will be used for keeping track of comment depth *)
    val depth = ref 0

  (* trimq : Substring.substring -> Substring.substring *) 
  (* purpose: to trim the quotes off of the given substring *)
  (*          e.g. "\"hammerhead\"" becomes "hammerhead" *)
    fun trimq ss = Substring.triml 1 (Substring.trimr 1 ss)

  (* keyword lookup table *)
    local
      val find = let
	    val tbl = AtomTable.mkTable (17, Fail "keywords")
	    fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
	    in
	      app ins [
		  ("alloc",	T.KW_alloc),
		  ("and",	T.KW_and),
		  ("any",	T.KW_any),
		  ("apply",	T.KW_apply),
		  ("bool",	T.KW_bool),
		  ("byte",	T.KW_byte),
		  ("case",	T.KW_case),
		  ("ccall",	T.KW_ccall),
		  ("cont",	T.KW_cont),
		  ("default",	T.KW_default),
		  ("double",	T.KW_double),
		  ("else",	T.KW_else),
		  ("end",	T.KW_end),
		  ("float",	T.KW_float),
		  ("fun",	T.KW_fun),
		  ("if",	T.KW_if),
		  ("int",	T.KW_int),
		  ("let",	T.KW_let),
		  ("long",	T.KW_long),
		  ("module",	T.KW_module),
		  ("short",	T.KW_short),
		  ("switch",	T.KW_switch),
		  ("then",	T.KW_then),
		  ("throw",	T.KW_throw),
		  ("unwrap",	T.KW_unwrap),
		  ("vec128",	T.KW_vec128),
		  ("wrap",	T.KW_wrap)
		];
	      AtomTable.find tbl
	    end
    in
  (* return either a keyword token or a ID token *)
    fun idToken id = let
	  val ida = Atom.atom id
	  in
	    case find ida
	     of NONE => T.ID ida
	      | SOME kw => kw
	    (* end case *)
	  end
    end
);

%states INITIAL COMMENT;

%let letter = [a-zA-Z];
%let dig = [0-9];
%let idchar = {letter}|{dig}|"_"|"'";
%let id = {letter}{idchar}*;
%let tyvarid = "'"{idchar}*;
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let ws = " "|[\t\n\v\f\r];

<INITIAL>"("	=> (T.LP);
<INITIAL>")"	=> (T.RP);
<INITIAL>"["	=> (T.LB);
<INITIAL>"]"	=> (T.RB);
<INITIAL>"#"	=> (T.HASH);
<INITIAL>","	=> (T.COMMA);
<INITIAL>"="	=> (T.EQ);
<INITIAL>":"	=> (T.COLON);
<INITIAL>";"	=> (T.SEMI);
<INITIAL>"-"	=> (T.MINUS);
<INITIAL>{id}	=> (idToken yytext);
<INITIAL>({dig})+
		=> (T.INT(valOf (IntInf.fromString yytext)));
<INITIAL>{ws}	=> (continue ());
<INITIAL>"(*"	=> (YYBEGIN COMMENT; depth := 1; continue());
<INITIAL> "\""({esc}|{sgood})*"\"" => 
  (T.STR(valOf (String.fromString (Substring.string (trimq yysubstr)))));
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
