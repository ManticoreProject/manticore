(* pml.lex
 *
 * COPYRIGHT (c) 2013 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

%name PMLLexer;

%arg ({source});

%defs(
  structure T = PMLTokens
  fun String_dropPrefix (s, n) = String.substring(s, n, size s - n)

  (* type pos = SourcePos.t *)
  type lex_arg = {source: Source.t}
  type lex_result = T.token

(* Different kinds of string literals.  In BOM code a normal string literal
 * is a BOM string and a string with a preceding "@" is treated as an
 * ML string literal.
 *)
  datatype string_type = CharLit | StringLit | MLStringLit

  val charlist: IntInf.int list ref = ref []
  val colNum: int ref = ref 0
  val commentLevel: int ref = ref 0
  val commentStart = ref SourcePos.bogus
  val lineFile: File.t ref = ref ""
  val lineNum: int ref = ref 0
  val stringStart = ref SourcePos.bogus
  val stringtype = ref CharLit

  local
    val bomLevel = ref 0
  in

  fun bomPush () =
    bomLevel := !bomLevel + 1

  fun bomPop () = let
	  val lvl = !bomLevel - 1
	in
	  bomLevel := lvl;
	  (lvl > 0)
	end

  fun inBOM () = (!bomLevel > 0)

  end (* local *)


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

  fun eof () = T.EOF

  fun int (yytext, drop, source, {negate: bool}, radix) =
    let
        (* DEBUG *)
      (* val _ = print (String.concat ["building int for text: ", yytext, "\n"]) *)
    in
     T.INT ({digits = (*String.dropPrefix*)String_dropPrefix (yytext, drop),
		  negate = negate,
		  radix = radix})
    end

  fun word (yytext, drop, source, radix : StringCvt.radix) =
     T.WORD ({digits = (*String.dropPrefix*)String_dropPrefix (yytext, drop),
		   radix = radix})

  fun trace (mark, s, token) = let
	val _ = print (String.concat [
		    mark, ": about to emit token ", T.toString token,
		    " for string \"", String.toString s, "\".\n"
		  ])
	in
	  token
	end
);

%states INITIAL A S F L LL LLC LLCQ BOM;

%let alphanum = [A-Za-z'_0-9]*;
%let alphanumId = [A-Za-z]{alphanum};
%let sym = [-!%&$+/:<=>?@~`\^|#*]|"\\";
%let symId = {sym}+;
%let id = {alphanumId}|{symId};
%let longid = ({alphanumId}\.)+{id};
%let hlid = "@"{alphanumId};
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
<INITIAL,BOM>"_"		=> (T.WILD);
<INITIAL,BOM>","		=> (T.COMMA);
<INITIAL,BOM>"{"		=> (T.LBRACE);
<INITIAL,BOM>"}"		=> (T.RBRACE);
<INITIAL,BOM>"["		=> (T.LBRACKET);
<INITIAL,BOM>"]"		=> (T.RBRACKET);
<INITIAL,BOM>";"		=> (T.SEMICOLON);
<INITIAL>"("			=> (T.LPAREN);
<INITIAL>")"			=> (T.RPAREN);
<BOM> "("			=> (bomPush(); T.LPAREN);
<BOM> ")"			=> (if bomPop() then () else YYBEGIN INITIAL; T.RPAREN);
<INITIAL>"..."			=> (T.DOTDOTDOT);
<INITIAL,BOM>"|"		=> (T.BAR);
<INITIAL,BOM>":"		=> (T.COLON);
<INITIAL>":>"			=> (T.COLONGT);
<INITIAL,BOM>"="		=> (trace ("146", yytext, T.EQUALOP));
<INITIAL,BOM>"#"		=> (T.HASH);
<INITIAL,BOM>"->"		=> (T.ARROW);
<INITIAL,BOM>"=>"		=> (T.DARROW);
<INITIAL,BOM>">"		=> (T.GT);
<INITIAL,BOM>"<"		=> (T.LT);
<INITIAL,BOM>"*"		=> (T.ASTERISK);
<INITIAL,BOM>"/"		=> (T.SLASH);

(* additional PML special symbols *)
<INITIAL>"(|"  			=> (T.PLPAREN);
<INITIAL>"|)"  			=> (T.PRPAREN);
<INITIAL>"{|"  			=> (T.PLBRACE);
<INITIAL>"|}"  			=> (T.PRBRACE);
<INITIAL>"[|"  			=> (T.PLBRACKET);
<INITIAL>"|]"  			=> (T.PRBRACKET);
<INITIAL>"?"			=> (T.PWILD);
<INITIAL,BOM>"&"		=> (T.AMPERSAND);

<BOM>"addr"			=> (T.KW_addr);
<BOM>"alloc"			=> (trace ("166", yytext, T.KW_alloc));
<INITIAL,BOM>"and"		=> (T.KW_and);
<INITIAL>"abstype"		=> (T.KW_abstype);
<BOM>"any"			=> (T.KW_any);
<BOM>"apply"			=> (T.KW_apply);
<INITIAL>"as"			=> (T.KW_as);
<INITIAL,BOM>"case"		=> (T.KW_case);
<BOM>"ccall"			=> (T.KW_ccall);
<BOM>"cont"			=> (T.KW_cont);
<INITIAL,BOM>"datatype"		=> (trace ("1", yytext, T.KW_datatype));
<BOM>"define"			=> (T.KW_define);
<INITIAL,BOM>"do"		=> (T.KW_do);
<INITIAL,BOM>"else"		=> (T.KW_else);
<INITIAL,BOM>"end"		=> (T.KW_end);
<INITIAL>"eqtype"		=> (T.KW_eqtype);
<INITIAL>"exception"		=> (T.KW_exception);
<BOM>"extern"			=> (T.KW_extern);
<INITIAL>"fn"			=> (T.KW_fn);
<INITIAL,BOM>"fun"		=> (T.KW_fun);
<INITIAL>"functor"		=> (T.KW_functor);
<INITIAL>"handle"		=> (T.KW_handle);
<BOM>"host_vproc"		=> (T.KW_host_vproc);
<INITIAL,BOM>"if"		=> (T.KW_if);
<INITIAL>"in"			=> (T.KW_in);
<INITIAL>"include"		=> (T.KW_include);
<INITIAL>"infix"		=> (T.KW_infix);
<INITIAL>"infixr"		=> (T.KW_infixr);
<BOM>"instance"			=> (T.KW_instance);
<INITIAL,BOM>"let"		=> (T.KW_let);
<INITIAL>"local"		=> (T.KW_local);
<INITIAL>"nonfix"		=> (T.KW_nonfix);
<BOM>"noreturn"			=> (T.KW_noreturn);
<BOM>"nullVP"			=> (trace ("198", yytext, T.KW_nullVP));
<INITIAL,BOM>"of"		=> (T.KW_of);
<INITIAL>"op"			=> (T.KW_op);
<INITIAL>"open"			=> (T.KW_open);
<INITIAL>"raise"		=> (T.KW_raise);
<INITIAL>"rec"			=> (T.KW_rec);
<BOM>"return"			=> (T.KW_return);
<INITIAL>"sharing"		=> (T.KW_sharing);
<INITIAL>"sig"			=> (T.KW_sig);
<INITIAL>"signature"		=> (T.KW_signature);
<INITIAL>"struct"		=> (T.KW_struct);
<INITIAL>"structure"		=> (T.KW_structure);
<INITIAL,BOM>"then"		=> (T.KW_then);
<BOM>"throw"			=> (T.KW_throw);
<INITIAL,BOM>"type"		=> (trace ("209", yytext, T.KW_type));
<BOM>"typecase"			=> (T.KW_typecase);
<INITIAL>"val"			=> (T.KW_val);
<BOM>"void"			=> (trace ("211", yytext, T.KW_void));
(* <BOM>"void*"			=> (trace ("212", yytext, T.KW_voidp)); *)
<BOM>"vpaddr"			=> (T.KW_vpaddr);
<BOM>"vproc"			=> (T.KW_vproc);
<BOM>"vpload"			=> (T.KW_vpload);
<BOM>"vpstore"			=> (T.KW_vpstore);
<INITIAL>"where"		=> (T.KW_where);
<INITIAL>"while"		=> (T.KW_while);
<INITIAL>"with"			=> (T.KW_with);
<INITIAL>"withtype"		=> (T.KW_withtype);
<INITIAL>"orelse"		=> (T.KW_orelse);
<INITIAL>"andalso"		=> (T.KW_andalso);
<BOM>"int8"			=> (trace ("222", yytext, T.KW_int8));
<BOM>"uint8"			=> (T.KW_uint8);
<BOM>"int16"			=> (T.KW_int16);
<BOM>"uint16"			=> (T.KW_uint16);
<BOM>"int32"			=> (T.KW_int32);
<BOM>"uint32"			=> (T.KW_uint32);
<BOM>"int64"			=> (T.KW_int64);
<BOM>"uint64"			=> (T.KW_uint64);
<BOM>"float32"			=> (T.KW_float32);
<BOM>"float64"			=> (T.KW_float64);

<INITIAL>"_address"		=> (T.KW__address);
<INITIAL>"_build_const"		=> (T.KW__build_const);
<INITIAL>"_command_line_const"	=> (T.KW__command_line_const);
<INITIAL>"_const"		=> (T.KW__const);
<INITIAL>"_export"		=> (T.KW__export);
<INITIAL>"_import"		=> (T.KW__import);
<INITIAL>"_overload"		=> (T.KW__overload);
<INITIAL>"_symbol"		=> (T.KW__symbol);
<INITIAL>"_prim"		=> (YYBEGIN BOM; T.KW__prim);
<INITIAL>"_primcode"		=> (YYBEGIN BOM; T.KW__primcode);
<INITIAL>"_datatype"		=> (T.KW__datatype);
<INITIAL>"_type"		=> (T.KW__type);
<INITIAL>"_val"			=> (T.KW__val);
<BOM> "__attributes__"		=> (T.KW___attributes__);
<BOM>":="			=> (T.ASSIGN);
(* <BOM>"$"			=> (T.DS); *)
<BOM>"#"			=> (T.HASH);
<BOM>"&"			=> (T.AMPERSAND);

<INITIAL,BOM>{symId}		=> (trace ("259", yytext, T.SYMID yytext));
<INITIAL,BOM>"'"{alphanum}?	=> (trace ("260", yytext, T.TYVAR yytext));
(* FIXME: split LONGID into unqualified id and qualified id *)
<INITIAL>{id}			=> (trace ("262", yytext, T.LONGID yytext));
<INITIAL,BOM>{longid}		=> (trace ("263", yytext, T.LONGID yytext));
<BOM>{alphanumId}		=> (trace ("264", yytext, T.ID yytext));
<BOM>({alphanumId}\.)+{id}	=> (trace ("265", yytext, T.LONGID yytext));
<BOM>{hlid}			=> (trace ("266", yytext, T.HLID yytext));
<BOM>({alphanumId}\.)+{hlid}	=> (trace ("267", yytext, T.LONG_HLID yytext));

<INITIAL,BOM>{real}		=> (T.REAL(yytext));
<INITIAL,BOM>{num}		=> (int (yytext, 0, source, {negate = false}, StringCvt.DEC));
<INITIAL,BOM>"~"{num}		=> (int (yytext, 1, source, {negate = true}, StringCvt.DEC));
<INITIAL,BOM>"0x"{hexnum}	=> (int (yytext, 2, source, {negate = false}, StringCvt.HEX));
<INITIAL,BOM>"~0x"{hexnum}	=> (int (yytext, 3, source, {negate = true}, StringCvt.HEX));
<INITIAL,BOM>"0w"{num}		=> (word (yytext, 2, source, StringCvt.DEC));
<INITIAL,BOM>"0wx"{hexnum}	=> (word (yytext, 3, source, StringCvt.HEX));
<INITIAL,BOM>\"     		=> (charlist := []
				    ; stringStart := Source.getPos (source, Position.toInt yypos)
				    ; stringtype := StringLit
				    ; YYBEGIN S
				    ; continue ());
<BOM>"@\""			=> (charlist := []
				    ; stringStart := Source.getPos (source, Position.toInt yypos)
				    ; stringtype := MLStringLit
				    ; YYBEGIN S
				    ; continue ());
<INITIAL,BOM>\#\"   		=> (charlist := []
				    ; stringStart := Source.getPos (source, Position.toInt yypos)
				    ; stringtype := CharLit
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
                    in
		      if inBOM() then YYBEGIN BOM else YYBEGIN INITIAL;
		      case !stringtype
		       of CharLit => if (Vector.length s <> 1)
			    then (
			      error (source, yypos, yypos + 1,
				"character literal not a single character");
			      T.CHAR 0)
			    else T.CHAR(Vector.sub (s, 0))
			| StringLit => T.STRING s
			| MLStringLit => T.ML_STRING s
		      (* end case *)
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
