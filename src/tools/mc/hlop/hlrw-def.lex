(* rw-def.lex
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 *)

%name HLRWDefLex;

%defs(
    structure T = HLRWDefTokens

  (* error function for lexers *)
    fun lexErr (lnum, msg) = TextIO.print(concat [
            "Error [line ", Int.toString lnum, "]: ", msg, "\n"
          ])

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token    

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

    val depth = ref 0
);

%states INITIAL COMMENT;

%let letter = [a-zA-Z];
%let dig = [0-9];
%let num = {dig}+;
%let idchar = {letter}|{dig}|"_"|"'";
%let id = ({letter}|"_"){idchar}*;
%let hlid = "@"{letter}({idchar}|"-")*;
%let eol = "\n"|"\r\n"|"\r";
%let ws = " "|[\t\v\f];

<INITIAL>"("            => (T.LP);
<INITIAL>")"            => (T.RP);
<INITIAL>"{"            => (T.LBRKT);
<INITIAL>"}"            => (T.RBRKT);
<INITIAL>";"            => (T.SEMI);
<INITIAL>","            => (T.COMMA);
<INITIAL>"==>"          => (T.DDARROW);
<INITIAL>{id}           => (T.ID (Atom.atom yytext));
<INITIAL>{hlid}         => (T.HLOP (Atom.atom yytext));
<INITIAL>{num}          => (T.NUM(valOf (InfInt.fromString yytext)));
<INITIAL>{ws}           => (skip());
<INITIAL>"(*"           => (YYBEGIN COMMENT; depth := 1; skip());
<INITIAL>{eol}          => (AntlrStreamPos.markNewLine yysm yypos; skip());
<INITIAL>.              => (lexErr(!yylineno, concat["bad character `",
                                                     String.toString yytext,
                                                     "'"]);
                            skip());

<COMMENT>"(*"           => (depth := !depth + 1; skip());
<COMMENT>"*)"           => (depth := !depth - 1;
                            if (!depth = 0) then YYBEGIN INITIAL else ();
                            skip());
<COMMENT>.              => (skip());
<COMMENT>{eol}          => (AntlrStreamPos.markNewLine yysm yypos; skip());
