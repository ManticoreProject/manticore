(* Heavily modified from the SML/NJ sources by sweeks@sweeks.com. *)

(* ml.lex
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * $Log: ml.lex,v $
 * Revision 1.3  1997/05/22  20:17:22  jhr
 * Changed lexer to accept "1e1" style floating-point literals.
 *
 * Revision 1.2  1997/01/28  23:20:40  jhr
 * Integer and word literals are now represented by IntInf.int (instead of
 * as strings).
 *
 *)

%name MLLexer;

%arg ({source});

%defs(
      (* type pos = SourcePos.t *)
type lex_arg = {source: Source.t}
type lex_result = Tokens.token

val charlist: IntInf.t list ref = ref []
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
   charlist :=
   String.fold (s, !charlist, fn (c, ac) => Int.toIntInf (Char.ord c) :: ac)

fun addChar (c: char) = addString (String.fromChar c)

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

fun addOrd (i: IntInf.t): unit = List.push (charlist, i)

fun addHexEscape (s: string, source, yypos): unit =
   case StringCvt.scanString (Pervasive.IntInf.scan StringCvt.HEX) s of
      NONE => stringError (source, yypos, "illegal unicode escape")
    | SOME i => addOrd i

fun eof () = Tokens.EOF

val size = String.size

      fun tok (t) = (
         if false
            then ()
         else
	     print (concat [Tokens.toString t,
			    ")\n"]);
	 t)

fun tok' (t, x, s, l) = t x

fun int (yytext, drop, source, {negate: bool}, radix) =
   Tokens.INT ({digits = String.dropPrefix (yytext, drop),
                negate = negate,
                radix = radix})

fun word (yytext, drop, source, radix : StringCvt.radix) =
   Tokens.WORD ({digits = String.dropPrefix (yytext, drop),
                 radix = radix})
);

%states INITIAL A S F L LL LLC LLCQ;

%let alphanum = [A-Za-z'_0-9]*;
%let alphanumId = [A-Za-z]{alphanum};
%let sym = [-!%&$+/:<=>?@~`\^|#*]|"\\";
%let symId = {sym}+;
%let id = {alphanumId}|{symId};
%let longid = {id}("."{id})*;
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

<INITIAL>{ws}   => (continue ());
<INITIAL>{eol}  => (Source.newline (source, Position.toInt yypos); continue ());
<INITIAL>"_address" => (Tokens.ADDRESS);
<INITIAL>"_build_const" => (Tokens.BUILD_CONST);
<INITIAL>"_command_line_const" => (Tokens.COMMAND_LINE_CONST);
<INITIAL>"_const" => (Tokens.CONST);
<INITIAL>"_export" => (Tokens.EXPORT);
<INITIAL>"_import" => (Tokens.IMPORT);
<INITIAL>"_overload" => (Tokens.OVERLOAD);
<INITIAL>"_symbol" => (Tokens.SYMBOL);
<INITIAL>"_prim" => (Tokens.PRIM);
<INITIAL>"_"    => (Tokens.WILD);
<INITIAL>","    => (Tokens.COMMA);
<INITIAL>"{"    => (Tokens.LBRACE);
<INITIAL>"}"    => (Tokens.RBRACE);
<INITIAL>"["    => (Tokens.LBRACKET);
<INITIAL>"]"    => (Tokens.RBRACKET);
<INITIAL>";"    => (Tokens.SEMICOLON);
<INITIAL>"("    => (Tokens.LPAREN);
<INITIAL>")"    => (Tokens.RPAREN);
<INITIAL>"..."  => (Tokens.DOTDOTDOT);
<INITIAL>"|" => (Tokens.BAR);
<INITIAL>":" => (Tokens.COLON);
<INITIAL>":>" => (Tokens.COLONGT);
<INITIAL>"=" => (Tokens.EQUALOP);
<INITIAL>"#" => (Tokens.HASH);
<INITIAL>"->" => (Tokens.ARROW);
<INITIAL>"=>" => (Tokens.DARROW);
<INITIAL>"and" => (Tokens.AND);
<INITIAL>"abstype" => (Tokens.ABSTYPE);
<INITIAL>"as" => (Tokens.AS);
<INITIAL>"case" => (Tokens.CASE);
<INITIAL>"datatype" => (Tokens.DATATYPE);
<INITIAL>"else" => (Tokens.ELSE);
<INITIAL>"end" => (Tokens.END);
<INITIAL>"eqtype" => (Tokens.EQTYPE);
<INITIAL>"exception" => (Tokens.EXCEPTION);
<INITIAL>"do" => (Tokens.DO);
<INITIAL>"fn" => (Tokens.FN);
<INITIAL>"fun" => (Tokens.FUN);
<INITIAL>"functor" => (Tokens.FUNCTOR);
<INITIAL>"handle" => (Tokens.HANDLE);
<INITIAL>"if" => (Tokens.IF);
<INITIAL>"in" => (Tokens.IN);
<INITIAL>"include" => (Tokens.INCLUDE);
<INITIAL>"infix" => (Tokens.INFIX);
<INITIAL>"infixr" => (Tokens.INFIXR);
<INITIAL>"let" => (Tokens.LET);
<INITIAL>"local" => (Tokens.LOCAL);
<INITIAL>"nonfix" => (Tokens.NONFIX);
<INITIAL>"of" => (Tokens.OF);
<INITIAL>"op" => (Tokens.OP);
<INITIAL>"open" => (Tokens.OPEN);
<INITIAL>"raise" => (Tokens.RAISE);
<INITIAL>"rec" => (Tokens.REC);
<INITIAL>"sharing" => (Tokens.SHARING);
<INITIAL>"sig" => (Tokens.SIG);
<INITIAL>"signature" => (Tokens.SIGNATURE);
<INITIAL>"struct" => (Tokens.STRUCT);
<INITIAL>"structure" => (Tokens.STRUCTURE);
<INITIAL>"then" => (Tokens.THEN);
<INITIAL>"type" => (Tokens.TYPE);
<INITIAL>"val" => (Tokens.VAL);
<INITIAL>"where" => (Tokens.WHERE);
<INITIAL>"while" => (Tokens.WHILE);
<INITIAL>"with" => (Tokens.WITH);
<INITIAL>"withtype" => (Tokens.WITHTYPE);
<INITIAL>"orelse" => (Tokens.ORELSE);
<INITIAL>"andalso" => (Tokens.ANDALSO);
<INITIAL>"'"{alphanum}? => (Tokens.TYVAR(yytext));
<INITIAL>{longid} => 
   (case yytext of
       "*" => Tokens.ASTERISK
    | _ => Tokens.LONGID(yytext));
<INITIAL>{real} => (Tokens.REAL(yytext));
<INITIAL>{num} => 
   (int (yytext, 0, source, {negate = false}, StringCvt.DEC));
<INITIAL>"~"{num} =>
   (int (yytext, 1, source, {negate = true}, StringCvt.DEC));
<INITIAL>"0x"{hexnum} =>
   (int (yytext, 2, source, {negate = false}, StringCvt.HEX));
<INITIAL>"~0x"{hexnum} =>
   (int (yytext, 3, source, {negate = true}, StringCvt.HEX));
<INITIAL>"0w"{num} =>
   (word (yytext, 2, source, StringCvt.DEC));
<INITIAL>"0wx"{hexnum} =>
   (word (yytext, 3, source, StringCvt.HEX));
<INITIAL>\"     => (charlist := []
                    ; stringStart := Source.getPos (source, Position.toInt yypos)
                    ; stringtype := true
                    ; YYBEGIN S
                    ; continue ());
<INITIAL>\#\"   => (charlist := []
                    ; stringStart := Source.getPos (source, Position.toInt yypos)
                    ; stringtype := false
                    ; YYBEGIN S
                    ; continue ());
<INITIAL>"(*#line"{nrws}
                => (YYBEGIN L
                    ; commentStart := Source.getPos (source, Position.toInt yypos)
                    ; commentLevel := 1
                    ; continue ());
<INITIAL>"(*"   => (YYBEGIN A
                    ; commentLevel := 1
                    ; commentStart := Source.getPos (source, Position.toInt yypos)
                    ; continue ());
<INITIAL>.      => (error (source, yypos, yypos + 1, "illegal token") ;
                    continue ());

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
                       val s = Vector.fromListRev (!charlist)
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

