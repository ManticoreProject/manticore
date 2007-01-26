(* cps.lex
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

%name CPSLex;

%defs(

    structure T = CPSParseToks

  (* error function for lexers *)
    fun lexErr (lnum, msg) = Error.say [
	    "Error [", !Error.sourceFile, ":", Int.toString lnum, "]: ", msg, "\n"
	  ]

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
		  ("alloc",	KW_alloc),
		  ("and",	KW_and),
		  ("any",	KW_any),
		  ("apply",	KW_apply),
		  ("bool",	KW_bool),
		  ("byte",	KW_byte),
		  ("case",	KW_case),
		  ("ccall",	KW_ccall),
		  ("cont",	KW_cont),
		  ("default",	KW_default),
		  ("double",	KW_double),
		  ("else",	KW_else),
		  ("end",	KW_end),
		  ("float",	KW_float),
		  ("fun",	KW_fun),
		  ("if",	KW_if),
		  ("int",	KW_int),
		  ("let",	KW_let),
		  ("long",	KW_long),
		  ("module",	KW_module),
		  ("short",	KW_short),
		  ("switch",	KW_switch),
		  ("then",	KW_then),
		  ("throw",	KW_throw),
		  ("unwrap",	KW_unwrap),
		  ("vec128",	KW_vec128),
		  ("wrap",	KW_wrap)
		];
	      AtomTable.find tbl
	    end
    in
  (* return either a keyword token or a NAME token *)
    fun idToken id = let
	  val ida = Atom.atom id
	  in
	    case find ida
	     of NONE => T.NAME ida
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
		=> (T.NUMBER(valOf (IntInf.fromString yytext)));
<INITIAL>{ws}	=> (continue ());
<INITIAL>"(*"	=> (YYBEGIN COMMENT; depth := 1; continue());
<INITIAL> "\""({esc}|{sgood})*"\"" => 
  (T.STRING 
   (valOf
    (String.fromString
     (Substring.string 
      (trimq 	
       (Substring.full yytext))))));
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
