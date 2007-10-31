(* hlop-def.lex
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

%name HLOpDefLex;

%defs(

    structure T = HLOpDefTokens

  (* error function for lexers *)
    fun lexErr (lnum, msg) = TextIO.print(concat [
	    "Error [line ", Int.toString lnum, "]: ", msg, "\n"
	  ])

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
      val find = let
	    val tbl = AtomTable.mkTable (17, Fail "keywords")
	    fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
	    in
	      app ins [
		  ("addr",	T.KW_addr),
		  ("alloc",	T.KW_alloc),
		  ("and",	T.KW_and),
		  ("any",	T.KW_any),
		  ("apply",	T.KW_apply),
		  ("__attribute__", T.KW_attribute),
		  ("byte",	T.KW_byte),
		  ("case",	T.KW_case),
		  ("ccall",	T.KW_ccall),
		  ("cont",	T.KW_cont),
		  ("datatype",	T.KW_datatype),
		  ("define",	T.KW_define),
		  ("do",	T.KW_do),
		  ("double",	T.KW_double),
		  ("else",	T.KW_else),
		  ("end",	T.KW_end),
		  ("enum",	T.KW_enum),
		  ("extern",	T.KW_extern),
		  ("float",	T.KW_float),
		  ("fun",	T.KW_fun),
		  ("galloc",	T.KW_galloc),
		  ("host_vproc", T.KW_host_vproc),
		  ("if",	T.KW_if),
		  ("inline",	T.KW_inline),
		  ("int",	T.KW_int),
		  ("let",	T.KW_let),
		  ("long",	T.KW_long),
		  ("module",	T.KW_module),
		  ("noreturn",	T.KW_noreturn),
		  ("of",	T.KW_of),
		  ("promote",	T.KW_promote),
		  ("pure",	T.KW_pure),
		  ("return",	T.KW_return),
		  ("short",	T.KW_short),
		  ("then",	T.KW_then),
		  ("throw",	T.KW_throw),
		  ("typedef",	T.KW_typedef),
		  ("unwrap",	T.KW_unwrap),
                  ("use_rw",    T.KW_use_rw),
		  ("vec128",	T.KW_vec128),
		  ("void",	T.KW_void),
		  ("vproc",	T.KW_vproc),
		  ("vpload",	T.KW_vpload),
		  ("vpstore",	T.KW_vpstore),
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

%states INITIAL COMMENT STRING;

%let letter = [a-zA-Z];
%let dig = [0-9];
%let num = {dig}+;
%let idchar = {letter}|{dig}|"_"|"'";
%let id = ({letter}|"_"){idchar}*;
%let hlid = "@"{letter}({idchar}|"-")*;
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let eol = "\n"|"\r\n"|"\r";
%let ws = " "|[\t\v\f];
%let filename = "\""{sgood}*"\"";

<INITIAL>"#"{ws}*{dig}+{ws}+{filename}({ws}+{dig}+)*{eol} => (
(*DEBUG
	print yytext;
DEBUG*)
	case RunCPP.parseLineDirective yytext
	 of SOME{lineNo, fileName} => 
	      AntlrStreamPos.resynch yysm (yypos, {fileName=fileName, lineNo=lineNo, colNo=1})
	  | _ => ()
	(* end case *);
	skip());

<INITIAL>"("		=> (T.LP);
<INITIAL>")"		=> (T.RP);
<INITIAL>"["		=> (T.LB);
<INITIAL>"]"		=> (T.RB);
<INITIAL>"#"		=> (T.HASH);
<INITIAL>"*"		=> (T.STAR);
<INITIAL>","		=> (T.COMMA);
<INITIAL>"="		=> (T.EQ);
<INITIAL>":"		=> (T.COLON);
<INITIAL>";"		=> (T.SEMI);
<INITIAL>"/"		=> (T.SLASH);
<INITIAL>"|"		=> (T.BAR);
<INITIAL>"_"		=> (T.WILD);
<INITIAL>"!"		=> (T.BANG);
<INITIAL>"&"		=> (T.AMP);
<INITIAL>"$"		=> (T.DS);
<INITIAL>":="		=> (T.ASSIGN);
<INITIAL>"=>"		=> (T.DARROW);
<INITIAL>"->"		=> (T.ARROW);
<INITIAL>{id}		=> (idToken yytext);
<INITIAL>{hlid}		=> (T.HLOP(Atom.atom(String.extract(yytext, 1, NONE))));
<INITIAL>{num}		=> (T.POSINT(valOf (IntInf.fromString yytext)));
<INITIAL>[~\045]{num}	=> (T.NEGINT(valOf (IntInf.fromString yytext)));
<INITIAL>[~\045]?{num}"."{num}([eE][+~\045]?{num})?
			=> (mkFloat yysubstr);
<INITIAL>{ws}		=> (skip ());
<INITIAL>{eol}		=> (AntlrStreamPos.markNewLine yysm yypos; skip());
<INITIAL>"(*"		=> (YYBEGIN COMMENT; depth := 1; skip());
<INITIAL> "\""		=> (YYBEGIN STRING; skip());

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
	skip());

<COMMENT> "(*" => (
	depth := !depth + 1;
	skip());
<COMMENT> "*)" => (
	depth := !depth - 1;
        if (!depth = 0) then YYBEGIN INITIAL else ();
	skip ());
<COMMENT>.		=> (skip ());
<COMMENT>{eol}		=> (AntlrStreamPos.markNewLine yysm yypos; skip());
