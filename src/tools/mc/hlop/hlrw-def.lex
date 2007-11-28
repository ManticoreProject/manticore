(* hlrw-def.lex
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

    (* make a FLOAT token from a substring *)
    val mkFloat = HLDefUtils.mkMkFloat (fn lit => T.FLOAT lit)

    (* idToken() - Create an identifier or a keyword. *)
    local
        val kwPairs = [
            ("addr",T.KW_addr),
            ("any", T.KW_any),
            ("byte", T.KW_byte),
            ("cont", T.KW_cont),
            ("double", T.KW_double),
            ("enum", T.KW_enum),
            ("float", T.KW_float),
            ("fun", T.KW_fun),
            ("int", T.KW_int),
            ("long", T.KW_long),
            ("short", T.KW_short),
            ("typedef", T.KW_typedef),
            ("vec128", T.KW_vec128),
            ("vproc", T.KW_vproc)
            ]
        fun ins ((id, tok), acc) = AtomMap.insert(acc, Atom.atom id, tok)
        val kwMap = List.foldl ins AtomMap.empty kwPairs
        fun find (id) = AtomMap.find(kwMap, id)
    in
    fun idToken id = (case find id
                       of SOME tok => tok
                        | NONE => T.ID id
                       (* end case *))
    end (* local *)
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
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let filename = "\""{sgood}*"\"";

<INITIAL>"#"{ws}*{dig}+{ws}+{filename}({ws}+{dig}+)*{eol} => (
        case RunCPP.parseLineDirective yytext
         of SOME{lineNo, fileName} => 
              AntlrStreamPos.resynch yysm (yypos, {fileName=fileName,
                                                   lineNo=lineNo, colNo=1})
          | _ => ()
        (* end case *);
        skip());

<INITIAL>"("            => (T.LP);
<INITIAL>")"            => (T.RP);
<INITIAL>"["            => (T.LB);
<INITIAL>"]"            => (T.RB);
<INITIAL>"{"            => (T.LBRKT);
<INITIAL>"}"            => (T.RBRKT);
<INITIAL>":"            => (T.COLON);
<INITIAL>";"            => (T.SEMI);
<INITIAL>","            => (T.COMMA);
<INITIAL>"="            => (T.EQ);
<INITIAL>"!"            => (T.BANG);
<INITIAL>"/"            => (T.SLASH);
<INITIAL>"->"           => (T.ARROW);
<INITIAL>"==>"          => (T.DDARROW);
<INITIAL>{id}           => (idToken (Atom.atom yytext));
<INITIAL>{hlid}         => (T.HLOP (Atom.atom yytext));
<INITIAL>{num}          => (T.POSINT(valOf (IntInf.fromString yytext)));
<INITIAL>[~\045]{num}   => (T.NEGINT(valOf (IntInf.fromString yytext)));
<INITIAL>[~\045]?{num}"."{num}([eE][+~\045]?{num})?
                        => (mkFloat yysubstr);
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
