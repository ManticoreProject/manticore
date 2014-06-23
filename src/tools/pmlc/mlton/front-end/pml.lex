(* pml.lex
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

%name PMLLexer;

%arg ({source});

%defs(
fun String_dropPrefix (s, n) = String.substring(s, n, size s - n)

      (* type pos = SourcePos.t *)
type lex_arg = {source: Source.t}
type lex_result = Tokens.token

val charlist: IntInf.int list ref = ref []
val colNum: int ref = ref 0
val commentLevel: int ref = ref 0
val commentStart = ref SourcePos.bogus
val lineFile: File.t ref = ref ""
val lineNum: int ref = ref 0
val stringStart = ref SourcePos.bogus
val stringtype = ref false

fun lineDirective (source, file, yypos) =
   Source.lineDirective (source, file,
                         {lineNum = !lineNum,
                                 lineStart = (Position.toInt yypos) - !colNum})

fun addString (s: string) =
   charlist := CharVector.foldl (fn (c, ac) => Int.toLarge (Char.ord c) :: ac) (!charlist) s

fun addChar (c: char) = charlist := Int.toLarge(Char.ord c) :: !charlist

fun inc (ri as ref (i: int)) = ri := i + 1

fun dec (ri as ref (i: int)) = ri := i - 1

fun error (source, left, right, msg) =
   Control.errorStr (Region.make {left = Source.getPos (source, Position.toInt left),
                                  right = Source.getPos (source, Position.toInt right)},
                     msg)

fun stringError (source, right, msg) =
   Control.errorStr (Region.make {left = !stringStart,
                                  right = Source.getPos (source, Position.toInt right)},
                     msg)

fun addOrd (i: IntInf.int): unit = MLtonList.push (charlist, i)

fun addHexEscape (s: string, source, yypos): unit =
   case StringCvt.scanString (Pervasive.IntInf.scan StringCvt.HEX) s of
      NONE => stringError (source, yypos, "illegal unicode escape")
    | SOME i => addOrd i

fun eof () = Tokens.EOF

fun int (yytext, drop, source, {negate: bool}, radix) =
   Tokens.INT ({digits = (*String.dropPrefix*)String_dropPrefix (yytext, drop),
                negate = negate,
                radix = radix})

fun word (yytext, drop, source, radix : StringCvt.radix) =
   Tokens.WORD ({digits = (*String.dropPrefix*)String_dropPrefix (yytext, drop),
                 radix = radix})
);

%states INITIAL A S F L LL LLC LLCQ BOM;

%let alphanum = [A-Za-z'_0-9]*;
%let alphanumId = [A-Za-z]{alphanum};
%let sym = [-!%&$+/:<=>?@~`\^|#*]|"\\";
%let symId = {sym}+;
%let id = {alphanumId}|{symId};
%let longid = ({alphanumId}.)*{id};
%let hlid = "@"{letter}({idchar}|"-")*;
%let ws = ("\012"|[\t\ ])*;
%let nrws = ("\012"|[\t\ ])+;
%let cr = "\013";
%let nl = "\010";
%let eol = ({cr}{nl}|{nl}|{cr});
%let num = [0-9]+;
%let frac = "."{num};
%let exp = [eE](\~?){num};
%let real = (\~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
%let hexDigit = [0-9a-fA-F];
%let hexnum = {hexDigit}+;

<INITIAL,BOM>{ws}		=> (skip ());
<INITIAL,BOM>{eol}		=> (Source.newline(source, Position.toInt yypos); skip ());
<INITIAL,BOM>"_"		=> (Tokens.WILD);
<INITIAL,BOM>","		=> (Tokens.COMMA);
<INITIAL,BOM>"{"		=> (Tokens.LBRACE);
<INITIAL,BOM>"}"		=> (Tokens.RBRACE);
<INITIAL,BOM>"["		=> (Tokens.LBRACKET);
<INITIAL,BOM>"]"		=> (Tokens.RBRACKET);
<INITIAL,BOM>";"		=> (Tokens.SEMICOLON);
<INITIAL>"("			=> (Tokens.LPAREN);
<INITIAL>")"			=> (Tokens.RPAREN);
<INITIAL,BOM>"..."		=> (Tokens.DOTDOTDOT);
<INITIAL,BOM>"|"		=> (Tokens.BAR);
<INITIAL,BOM>":"		=> (Tokens.COLON);
<INITIAL,BOM>":>"		=> (Tokens.COLONGT);
<INITIAL,BOM>"="		=> (Tokens.EQUALOP);
<INITIAL,BOM>"#"		=> (Tokens.HASH);
<INITIAL,BOM>"->"		=> (Tokens.ARROW);
<INITIAL,BOM>"=>"		=> (Tokens.DARROW);

(* additional PML special symbols *)
<INITIAL>"(|"  			=> (T.PLPAREN);
<INITIAL>"|)"  			=> (T.PRPAREN);
<INITIAL>"{|"  			=> (T.PLBRACE);
<INITIAL>"|}"  			=> (T.PRBRACE);
<INITIAL>"[|"  			=> (T.PLBRACKET);
<INITIAL>"|]"  			=> (T.PRBRACKET);
<INITIAL>"?"			=> (T.PWILD);
<INITIAL,BOM>"&"		=> (T.AMP);

<INITIAL>"and"			=> (Tokens.KW_and);
<INITIAL>"abstype"		=> (Tokens.KW_abstype);
<INITIAL>"as"			=> (Tokens.KW_as);
<INITIAL>"case"			=> (Tokens.KW_case);
<INITIAL>"datatype"		=> (Tokens.KW_datatype);
<INITIAL>"else"			=> (Tokens.KW_else);
<INITIAL>"end"			=> (Tokens.KW_end);
<INITIAL>"eqtype"		=> (Tokens.KW_eqtype);
<INITIAL>"exception"		=> (Tokens.KW_exception);
<INITIAL>"do"			=> (Tokens.KW_do);
<INITIAL>"fn"			=> (Tokens.KW_fn);
<INITIAL>"fun"			=> (Tokens.KW_fun);
<INITIAL>"functor"		=> (Tokens.KW_functor);
<INITIAL>"handle"		=> (Tokens.KW_handle);
<INITIAL>"if"			=> (Tokens.KW_if);
<INITIAL>"in"			=> (Tokens.KW_in);
<INITIAL>"include"		=> (Tokens.KW_include);
<INITIAL>"infix"		=> (Tokens.KW_infix);
<INITIAL>"infixr"		=> (Tokens.KW_infixr);
<INITIAL>"let"			=> (Tokens.KW_let);
<INITIAL>"local"		=> (Tokens.KW_local);
<INITIAL>"nonfix"		=> (Tokens.KW_nonfix);
<INITIAL>"of"			=> (Tokens.KW_of);
<INITIAL>"op"			=> (Tokens.KW_op);
<INITIAL>"open"			=> (Tokens.KW_open);
<INITIAL>"raise"		=> (Tokens.KW_raise);
<INITIAL>"rec"			=> (Tokens.KW_rec);
<INITIAL>"sharing"		=> (Tokens.KW_sharing);
<INITIAL>"sig"			=> (Tokens.KW_sig);
<INITIAL>"signature"		=> (Tokens.KW_signature);
<INITIAL>"struct"		=> (Tokens.KW_struct);
<INITIAL>"structure"		=> (Tokens.KW_structure);
<INITIAL>"then"			=> (Tokens.KW_then);
<INITIAL>"type"			=> (Tokens.KW_type);
<INITIAL>"val"			=> (Tokens.KW_val);
<INITIAL>"where"		=> (Tokens.KW_where);
<INITIAL>"while"		=> (Tokens.KW_while);
<INITIAL>"with"			=> (Tokens.KW_with);
<INITIAL>"withtype"		=> (Tokens.KW_withtype);
<INITIAL>"orelse"		=> (Tokens.KW_orelse);
<INITIAL>"andalso"		=> (Tokens.KW_andalso);

<INITIAL>"_address"		=> (Tokens.KW__address);
<INITIAL>"_build_const"		=> (Tokens.KW__build_const);
<INITIAL>"_command_line_const"	=> (Tokens.KW__command_line_const);
<INITIAL>"_const"		=> (Tokens.KW__const);
<INITIAL>"_export"		=> (Tokens.KW__export);
<INITIAL>"_import"		=> (Tokens.KW__import);
<INITIAL>"_overload"		=> (Tokens.KW__overload);
<INITIAL>"_symbol"		=> (Tokens.KW__symbol);
<INITIAL> "_primcode"		=> (YYBEGIN BOM; T.KW__primcode);
<INITIAL> "_prim"		=> (YYBEGIN BOM; T.KW__prim);
<BOM> "__attribute__"		=> (T.KW___attribute__);
<BOM> "("			=> (bomPush()(); T.LP);
<BOM> ")"			=> (if bomPop() then () else YYBEGIN INITIAL; T.RP);
<BOM>":="			=> (T.ASSIGN);
<BOM>"$"			=> (T.DS);
<BOM>"#"			=> (T.HASH);
<BOM>"&"			=> (T.AMP);

<INITIAL,BOM>"'"{alphanum}?	=> (Tokens.TYVAR(yytext));
(* FIXME: split LONGID into unqualified id and qualified id *)
<INITIAL,BOM>{longid}		=> (case yytext
				     of "*" => Tokens.ASTERISK
   				      | _ => Tokens.LONGID(yytext)
				    (* end case *));
<BOM>{hlid}			=> (Tokens.HLOPID(Atom.atom yytext))

<INITIAL>{real}			=> (Tokens.REAL(yytext));
<INITIAL>{num}			=> (int (yytext, 0, source, {negate = false}, StringCvt.DEC));
<INITIAL>"~"{num}		=> (int (yytext, 1, source, {negate = true}, StringCvt.DEC));
<INITIAL>"0x"{hexnum}		=> (int (yytext, 2, source, {negate = false}, StringCvt.HEX));
<INITIAL>"~0x"{hexnum}		=> (int (yytext, 3, source, {negate = true}, StringCvt.HEX));
<INITIAL>"0w"{num}		=> (word (yytext, 2, source, StringCvt.DEC));
<INITIAL>"0wx"{hexnum}		=> (word (yytext, 3, source, StringCvt.HEX));
<INITIAL>\"     		=> (charlist := []
				    ; stringStart := Source.getPos (source, Position.toInt yypos)
				    ; stringtype := true
				    ; YYBEGIN S
				    ; continue ());
<INITIAL>\#\"   		=> (charlist := []
				    ; stringStart := Source.getPos (source, Position.toInt yypos)
				    ; stringtype := false
				    ; YYBEGIN S
				    ; continue ());
<INITIAL>"(*#line"{nrws}	=> (YYBEGIN L
				    ; commentStart := Source.getPos (source, Position.toInt yypos)
				    ; commentLevel := 1
				    ; continue ());
<INITIAL>"(*"   		=> (YYBEGIN A
				    ; commentLevel := 1
				    ; commentStart := Source.getPos (source, Position.toInt yypos)
				    ; continue ());
<INITIAL>.      		=> (error (source, yypos, yypos + 1, "illegal token")
				    ; continue ());

<L>[0-9]+       => (YYBEGIN LL
                    ; (lineNum := valOf (Int.fromString yytext)
                       ; colNum := 1)
                      handle Overflow => YYBEGIN A
                    ; continue ());
<LL>\.          => ((* cheat: take n > 0 dots *) continue ());
<LL>[0-9]+      => (YYBEGIN LLC
                    ; (colNum := valOf (Int.fromString yytext))
                      handle Overflow => YYBEGIN A
                    ; continue ());
<LL>.          => (YYBEGIN LLC; continue ()
                (* note hack, since ml-lex chokes on the empty string for 0* *));
<LLC>"*)"       => (YYBEGIN INITIAL
                    ; lineDirective (source, NONE, yypos + 2)
                    ; commentLevel := 0; charlist := []; continue ());
<LLC>{ws}\"     => (YYBEGIN LLCQ; continue ());
<LLCQ>[^\"]*    => (lineFile := yytext; continue ());
<LLCQ>\""*)"    => (YYBEGIN INITIAL
                    ; lineDirective (source, SOME (!lineFile), yypos + 3)
                    ; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>"*)" => (YYBEGIN INITIAL; commentLevel := 0; charlist := []; continue ());
<L,LLC,LLCQ>.   => (YYBEGIN A; continue ());

<A>"(*"         => (inc commentLevel; continue ());
<A>\n           => (Source.newline (source, Position.toInt yypos) ; continue ());
<A>"*)"         => (dec commentLevel
                    ; if 0 = !commentLevel then YYBEGIN INITIAL else ()
                    ; continue ());
<A>.            => (continue ());

<S>\"           => (let
                       val s = MLtonVector.fromListRev (!charlist)
                       val _ = charlist := nil
                       fun make (t, v) =
                          t (v)
                       val () = YYBEGIN INITIAL
                    in
                       if !stringtype
                          then make (Tokens.STRING, s)
                       else
                          make (Tokens.CHAR,
                                if 1 <> Vector.length s
                                   then (error
                                         (source, yypos, yypos + 1,
                                          "character constant not length 1")
                                         ; 0)
                                else Vector.sub (s, 0))
                    end);
<S>\\a          => (addChar #"\a"; continue ());
<S>\\b          => (addChar #"\b"; continue ());
<S>\\f          => (addChar #"\f"; continue ());
<S>\\n          => (addChar #"\n"; continue ());
<S>\\r          => (addChar #"\r"; continue ());
<S>\\t          => (addChar #"\t"; continue ());
<S>\\v          => (addChar #"\v"; continue ());
<S>\\\^[@-_]    => (addChar (Char.chr(Char.ord(String.sub(yytext, 2))
                                      -Char.ord #"@"));
                    continue ());
<S>\\\^.        =>
        (error (source, yypos, yypos + 2,
                "illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
        continue ());
<S>\\[0-9]{3}   => (let
                       fun c (i, scale) =
                          scale * (Char.ord (String.sub (yytext, i))
                                   - Char.ord #"0")
                       val () = addOrd (IntInf.fromInt
                                        (c (1, 100) + c (2, 10) + c (3, 1)))
                    in
                       continue ()
                    end);
<S>\\u{hexDigit}{4} => (addHexEscape (String.substring (yytext, 2, 4),
                                      source, yypos)
                        ; continue ());
<S>\\U{hexDigit}{8} => (addHexEscape (String.substring (yytext, 2, 8),
                                      source, yypos)
                        ; continue ());
<S>\\\"         => (addString "\""; continue ());
<S>\\\\         => (addString "\\"; continue ());
<S>\\{nrws}     => (YYBEGIN F; continue ());
<S>\\{eol}      => (Source.newline (source, (Position.toInt yypos) + 1) ; YYBEGIN F ; continue ());
<S>\\           => (stringError (source, yypos, "illegal string escape")
                    ; continue ());
<S>{eol}        => (Source.newline (source, Position.toInt yypos)
                    ; stringError (source, yypos, "unclosed string")
                    ; continue ());
<S>" "|[\033-\126]  => (addString yytext; continue ());
<S>. =>  (stringError (source, Position.fromInt (Position.toInt (yypos) + 1), "illegal character in string")
          ; continue ());

<F>{eol}        => (Source.newline (source, Position.toInt yypos) ; continue ());
<F>{ws}         => (continue ());
<F>\\           => (YYBEGIN S
                    ; stringStart := Source.getPos (source, Position.toInt yypos)
                    ; continue ());
<F>.            => (stringError (source, yypos, "unclosed string")
                    ; continue ());
