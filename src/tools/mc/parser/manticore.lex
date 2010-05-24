(* manticore.lex
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * The unified lexer for SML and inline BOM code.
 *)

%name ManticoreLex;

%arg (lexErr);

%defs(

    structure T = ManticoreTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* the depth int ref will be used for keeping track of comment depth *)
    val depth = ref 0

  (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

  (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

  (* flag to mark ML string literals.  In BOM code a normal string literal
   * is a BOM string and a string with a preceding "@" is treated as an
   * ML string literal.
   *)
    val isMLString = ref false

  (* make a string from buf *)
    fun mkString () = let
	  val s = String.concat(List.rev(!buf))
	  in
	    buf := [];
	    if !isMLString
	      then T.ML_STRING s
	      else T.STRING s
	  end

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

  (* scan a number from a hexidecimal string *)
    val fromHexString = valOf o (StringCvt.scanString (IntInf.scan StringCvt.HEX))
(* FIXME: the above code doesn't work in SML/NJ; here is a work around *)
fun fromHexString s = let
      val SOME(n, _) = IntInf.scan StringCvt.HEX Substring.getc
	    (Substring.triml 2 (Substring.full s))
      in
	n
      end

  (* convert a HLOp ID to an atom *)
    fun cvtHLOpId id = Atom.atom(String.extract(id, 1, NONE))

  (* split a qualified ID up into its prefix and id parts *)
    fun makeQualifiedId cvtId id = let
	  fun revMap ([], l) = l
	    | revMap (x::xs, l) = revMap(xs, Atom.atom x :: l)
	  in
	    case List.rev (String.tokens (fn c => c = #".") id)
	      of (id::path) => (revMap (path, []), cvtId id)
	       | _ => raise Fail "bogus qualified ID"
	    (* end case *)
	  end

    val mkQId = makeQualifiedId Atom.atom
    val mkQHLOpId = makeQualifiedId cvtHLOpId

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

  (* count the nesting depth of "(" inside primcode blocks *)
    val primDepth = ref 0

    fun inPrimCode () = (!primDepth > 0)
    fun primPush () = (primDepth := !primDepth + 1)
    fun primPop () = let val p = !primDepth-1 in primDepth := p; (p > 0) end
);

%states INITIAL STRING COMMENT PRIMCODE;

%let letter = [a-zA-Z];
%let dig = [0-9];
%let num = {dig}+;
%let hexdigit = [0-9a-fA-F];
%let hexnum = "0x"{hexdigit}+;
%let idchar = {letter}|{dig}|"_"|"'";
%let id = {letter}{idchar}*;
%let qidt = {id}".";
%let qualifiedid = {qidt}+{id};
%let tyvarid = "'"{idchar}*;
%let hlid = "@"{letter}({idchar}|"-")*;
%let qualifiedhlid = {qidt}+{hlid};
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let ws = " "|[\t\n\v\f\r];

<INITIAL,PRIMCODE> "["	=> (T.LB);
<INITIAL,PRIMCODE> "]"	=> (T.RB);
<INITIAL,PRIMCODE> "{"	=> (T.LCB);
<INITIAL,PRIMCODE> "}"	=> (T.RCB);
<INITIAL,PRIMCODE> "->"	=> (T.ARROW);
<INITIAL,PRIMCODE> "=>"	=> (T.DARROW);
<INITIAL,PRIMCODE> "_"	=> (T.WILD);
<INITIAL,PRIMCODE> "!"   => (T.PSUB);
<INITIAL,PRIMCODE> "*"	=> (T.TIMES);
<INITIAL,PRIMCODE> "/"	=> (T.SLASH);
<INITIAL,PRIMCODE> "="	=> (T.EQ);
<INITIAL,PRIMCODE> ","	=> (T.COMMA);
<INITIAL,PRIMCODE> ";"	=> (T.SEMI);
<INITIAL,PRIMCODE> "|"	=> (T.BAR);
<INITIAL,PRIMCODE> ":"	=> (T.COLON);
<INITIAL,PRIMCODE> "&"   => (T.AMP);

<INITIAL> "("	=> (T.LP);
<INITIAL> ")"	=> (T.RP);
<INITIAL> "(|"  => (T.LPB);
<INITIAL> "|)"  => (T.RPB);
<INITIAL> "[|"  => (T.LBB);
<INITIAL> "|]"  => (T.RBB);
<INITIAL> "<="	=> (T.LTEQ);
<INITIAL> "<"	=> (T.LT);
<INITIAL> "<>"  => (T.NEQ);
<INITIAL> ">"   => (T.GT);
<INITIAL> ">="  => (T.GTEQ);
<INITIAL> "::"	=> (T.DCOLON);
<INITIAL> "@"	=> (T.AT);
<INITIAL> "^"	=> (T.CONCAT);
<INITIAL> "+"	=> (T.PLUS);
<INITIAL> "-"	=> (T.MINUS);
<INITIAL> "~"	=> (T.TILDE);
<INITIAL> ":>"	=> (T.SEAL);
<INITIAL> "?"   => (T.NDWILD);
<INITIAL> "|?|" => (T.PCHOICE);

<INITIAL> "_primcode"		=> (YYBEGIN PRIMCODE; T.KW__primcode);
<INITIAL> "_prim"		=> (YYBEGIN PRIMCODE; T.KW__prim);

<INITIAL> {qualifiedid}		=> (T.QID(mkQId yytext));
<INITIAL> {id}			=> (Keywords.smlIdToken yytext);
<INITIAL> {tyvarid}		=> (T.TYVAR(Atom.atom yytext));
<INITIAL,PRIMCODE> {num}	=> (T.POSINT(valOf (IntInf.fromString yytext)));
<INITIAL,PRIMCODE> "~"{num}	=> (T.NEGINT(valOf (IntInf.fromString yytext)));
<INITIAL,PRIMCODE> "~"?{num}"."{num}([eE][+~]?{num})?
				=> (mkFloat yysubstr);
<INITIAL,PRIMCODE> {hexnum}	=> (T.POSINT(fromHexString yytext));
<INITIAL,PRIMCODE> {ws}		=> (skip ());
<INITIAL,PRIMCODE> "(*"		=> (YYBEGIN COMMENT; depth := 1; skip());
<PRIMCODE> "@\""		=> (isMLString := true; YYBEGIN STRING; skip());
<INITIAL,PRIMCODE> "\""		=> (isMLString := false; YYBEGIN STRING; skip());

<STRING>{esc}		=> (addStr(valOf(String.fromString yytext)); continue());
<STRING>{sgood}+	=> (addStr yytext; continue());
<STRING>"\""		=> (if inPrimCode() then YYBEGIN PRIMCODE else YYBEGIN INITIAL; mkString());
<STRING>"\\".		=> (lexErr(yypos, [
				"bad escape character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue());
<STRING>.		=> (lexErr(yypos, [
				"bad character `", String.toString yytext,
				"' in string literal"
			      ]);
			    continue());

<COMMENT> "(*"			=> (
	depth := !depth + 1;
	skip());
<COMMENT> "*)"			=> (
	depth := !depth - 1;
        if (!depth = 0) then if inPrimCode() then YYBEGIN PRIMCODE else YYBEGIN INITIAL else ();
	skip ());
<COMMENT> .|"\n"		=> (skip ());

<PRIMCODE> {id}			=> (Keywords.bomIdToken yytext);
<PRIMCODE> {qualifiedhlid}	=> (T.QHLOP(mkQHLOpId yytext));
<PRIMCODE> {qualifiedid}	=> (T.QID(mkQId yytext));
<PRIMCODE> {hlid}		=> (T.HLOP(cvtHLOpId yytext));
<PRIMCODE> "("			=> (primPush(); T.LP);
<PRIMCODE> ")"			=> (if primPop() then () else YYBEGIN INITIAL; T.RP);
<PRIMCODE> "__attribute__"	=> (T.KW___attribute__);
<PRIMCODE>":="			=> (T.ASSIGN);
<PRIMCODE>"$"			=> (T.DS);
<PRIMCODE>"#"			=> (T.HASH);

<INITIAL,PRIMCODE> . => (
	lexErr(yypos, ["bad character `", String.toString yytext, "'"]);
	continue());
