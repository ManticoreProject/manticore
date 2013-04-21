(* mlb.lex
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This code is based, in part, on the ml-lex specification in the MLton compiler, which is
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

%name MLBLexer;

%arg ({source});

%defs(
  type lex_arg = {source: Source.t}
  type lex_result = MLBTokens.token
  fun eof () = MLBTokens.EOF

  val charlist : string list ref = ref []
  val colNum : int ref = ref 0
  val commentLevel : int ref = ref 0
  val commentStart = ref SourcePos.bogus
  val lineFile : File.t ref = ref ""
  val lineNum : int ref = ref 0
  val stringStart = ref SourcePos.bogus

  fun lineDirective (source, file, yypos) =
	Source.lineDirective (source, file, {lineNum = !lineNum, lineStart = yypos - !colNum})
  fun addString (s: string) = charlist := s :: (!charlist)
  fun addChar (c: char) = addString (String.str c)

  fun inc (ri as ref (i: int)) = ri := i + 1
  fun dec (ri as ref (i: int)) = ri := i - 1

  fun error (source, left, right, msg) = Control.errorStr (
	Region.make {
	    left = Source.getPos (source, Position.toInt left),
	    right = Source.getPos (source, Position.toInt right)
	  },
	  msg)
  fun stringError (source, right, msg) = Control.errorStr (
	Region.make {
	    left = !stringStart,
	    right = Source.getPos (source, Position.toInt right)
	  },
	  msg)
);

%let alphanum=[A-Za-z'_0-9]*;
%let alphanumId=[A-Za-z]{alphanum};
%let id={alphanumId};

%let pathvar="$("([A-Z_][A-Z0-9_]*)")";
%let filename=({pathvar}|[A-Za-z0-9_.])({pathvar}|[-A-Za-z0-9_.])*;
%let arc=({pathvar}|{filename}|"."|"..");
%let relpath=({arc}"/")*;
%let abspath="/"{relpath};
%let path={relpath}|{abspath};
%let file={path}{filename};

%let ws=("\012"|[\t\ ])*;
%let nrws=("\012"|[\t\ ])+;
%let cr="\013";
%let nl="\010";
%let eol=({cr}{nl}|{nl}|{cr});

%let hexDigit=[0-9a-fA-F];

%states INITIAL A S F L LL LLC LLCQ;

<INITIAL>{ws}		=> (skip ());
<INITIAL>{eol}		=> (skip ());
<INITIAL>"_prim"	=> (MLBTokens.PRIM);
<INITIAL>","		=> (MLBTokens.COMMA);
<INITIAL>";"		=> (MLBTokens.SEMICOLON);
<INITIAL>"="		=> (MLBTokens.EQUALOP);
<INITIAL>"ann"		=> (MLBTokens.ANN);
<INITIAL>"and"  	=> (MLBTokens.AND);
<INITIAL>"bas"  	=> (MLBTokens.BAS);
<INITIAL>"basis"	=> (MLBTokens.BASIS);
<INITIAL>"end"  	=> (MLBTokens.END);
<INITIAL>"functor"	=> (MLBTokens.FUNCTOR);
<INITIAL>"in"   	=> (MLBTokens.IN);
<INITIAL>"let"  	=> (MLBTokens.LET);
<INITIAL>"local"	=> (MLBTokens.LOCAL);
<INITIAL>"open" 	=> (MLBTokens.OPEN);
<INITIAL>"signature"	=> (MLBTokens.SIGNATURE);
<INITIAL>"structure"	=> (MLBTokens.STRUCTURE);

<INITIAL>{id}		=> (MLBTokens.ID yytext);
<INITIAL>{file} 	=> (MLBTokens.FILE yytext);

<INITIAL>\"     => (charlist := []
                    ; stringStart := Source.getPos (source, Position.toInt yypos)
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
                       val s = concat (rev (!charlist))
                       val _ = charlist := nil
                    in
		      YYBEGIN INITIAL; MLBTokens.STRING s
                    end);
<S>\\a          => (addChar #"\a"; continue ());
<S>\\b          => (addChar #"\b"; continue ());
<S>\\f          => (addChar #"\f"; continue ());
<S>\\n          => (addChar #"\n"; continue ());
<S>\\r          => (addChar #"\r"; continue ());
<S>\\t          => (addChar #"\t"; continue ());
<S>\\v          => (addChar #"\v"; continue ());
<S>\\\^[@-_]    => (addChar (Char.chr(Char.ord(String.sub(yytext, 2))-Char.ord #"@"));
                    continue ());
<S>\\\^.        =>
        (error (source, yypos, yypos + 2,
                "illegal control escape; must be one of @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_");
        continue ());
<S>\\[0-9]{3}   => (let
                       val x =
                          Char.ord(String.sub(yytext, 1)) * 100
                          + Char.ord(String.sub(yytext, 2)) * 10
                          + Char.ord(String.sub(yytext, 3))
                          - (Char.ord #"0") * 111
                    in (if x > 255
                           then stringError (source, yypos,
                                             "illegal ascii escape")
                        else addChar(Char.chr x);
                           continue ())
                    end);
<S>\\u{hexDigit}{4} 
                => (let
                       val x = 
                          StringCvt.scanString
                          (Pervasive.Int.scan StringCvt.HEX)
                          (String.substring (yytext, 2, 4))
                       fun err () =
                          stringError (source, yypos,
                                       "illegal unicode escape")
                    in (case x of
                          SOME x => if x > 255
                                       then err()
                                    else addChar(Char.chr x)
                        | _ => err())
                        ; continue ()
                    end);
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
