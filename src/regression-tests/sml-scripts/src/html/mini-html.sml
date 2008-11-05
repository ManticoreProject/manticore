(* mini-html.sml
 *
 * COPYRIGHT (c) 2008 Adam Shaw (http://people.cs.uchicago.edu/~adamshaw)
 * All rights reserved.
 * 
 * This is a restricted model of HTML.
 * It is intended to be used by other programs, not people!
 * It is designed to be easily extensible.
 * In the generated html, it is assumed that _all_ formatting is 
 *   handled via stylesheets.
 *
 * There is not yet any corresponding abstract stylesheet module.
 * I'm still pondering whether or not that's necessary or desirable.
 *
 * An example use of this module: 
 * Say you have a module defining a type dataset.
 * You might write a function
 *   f : dataset -> MiniHTML.html
 * and then use MiniHTML.toFile to generate a web page from that. 
 *)

structure MiniHTML : MINI_HTML = struct

  structure TL = TagLanguage

  type tag_name = TL.tag_name
  type class_name = TL.tag_name
  type attr = TL.attr

  datatype term = datatype TL.term

(* from TL: *)
(* datatype term *)
(*   = SingletonTag of tag_name * class_name option * attr list *)
(*   | ContainerTag of tag_name * class_name option * attr list * term *)
(*   | Sequence of term list *)
(*   | String of string *)
(*   | Comment of string *)

  type html = term

  val sequence = Sequence

  fun escapeChar (char, escText) s = let
    val cs = explode s
    val es = explode escText
    fun loop [] = []
      | loop (c::cs) = if (c = char)
		       then es @ (loop cs)
		       else c :: (loop cs)
    in
      implode (loop cs)
    end

  val escString = foldr (op o) (fn x => x) [escapeChar (#"<", "&lt;"),
					    escapeChar (#">", "&gt;")]
(* escapeChar (#"&", "&amp;")] *)

(* esc : term -> term *)
(* HTML-escapes the plain text in String terms. *)
(* Assumes tag names, class names, attrs don't require escaping. (FIXME?) *)
(* This is NOT an idempotent operation, so handle with care. *)
(* Namely, & -> &amp; -> &ampamp; -> ... *)
  fun esc (s as SingletonTag _) = s
    | esc (c as ContainerTag _) = c
    | esc (Sequence ts) = Sequence (map esc ts)
    | esc (String s) = String (escString s)
    | esc (c as Comment _) = c
 
(* mk : string -> class_name option * term -> term *)
(* makes an atrribute-less tag maker *)
  fun mk tagName (optC, t) = ContainerTag (tagName, optC, [], esc t)

  val str = String

(* includeFile : string -> term *)
  fun includeFile infile = let
    val ins = TextIO.openIn infile
    fun get acc = (case TextIO.inputLine ins
      of NONE => (String o concat o rev) acc
       | SOME s => get (s :: acc)
      (* end case *))
    in
      get [] before TextIO.closeIn ins 
    end

  val nbsp = str "&nbsp;"

(* mkMakers : string -> (string -> term) *
 *                      (term -> term) *
 *                      (class_name * string -> term) *
 *                      (class_name * term -> term)
 *)
  fun mkMakers tagName = (fn s => mk tagName (NONE, str s),
			  fn h => mk tagName (NONE, h),
			  fn (c, s) => mk tagName (SOME c, str s),
			  fn (c, h) => mk tagName (SOME c, h))
			   

(* The convention followed below is
 *   h1   : string -> term
 *   h1H  : term -> term                (H for html)
 *   h1CS : class_name * string -> term (C for class, S for string)
 *   h1CH : class_name * term -> term   (C for class, H for html)
 *)
  val (h1, h1H, h1CS, h1CH) = mkMakers "h1"
  val (h2, h2H, h2CS, h2CH) = mkMakers "h2"
  val (h3, h3H, h3CS, h3CH) = mkMakers "h3"
  val (h4, h4H, h4CS, h4CH) = mkMakers "h4"
  val (h5, h5H, h5CS, h5CH) = mkMakers "h5"
  val (h6, h6H, h6CS, h6CH) = mkMakers "h6"

  val (mkDiv, divH, divCS, divCH) = mkMakers "div" (* the id div is already taken *)

  fun divCAH (c, attrs, h) = ContainerTag ("div", SOME c, attrs, h)

  val (span, spanH, spanCS, spanCH) = mkMakers "span"
  val (p, pH, pCS, pCH) = mkMakers "p"
  val (blockquote, blockquoteH, blockquoteCS, blockquoteCH) = mkMakers "blockquote"

  val (table, tableH, tableCS, tableCH) = mkMakers "table"
  val (tr, trH, trCS, trCH) = mkMakers "tr"
  val (td, tdH, tdCS, tdCH) = mkMakers "td"
  val (th, thH, thCS, thCH) = mkMakers "th"
  val (caption, captionH, captionCS, captionCH) = mkMakers "caption"

  val (ul, ulH, ulCS, ulCH) = mkMakers "ul"
  val (ol, olH, olCS, olCH) = mkMakers "ol"
  val (li, liH, liCS, liCH) = mkMakers "li"

  val (code, codeH, codeCS, codeCH) = mkMakers "code"

  val br = SingletonTag ("br", NONE, [])
  fun hr optC = SingletonTag ("hr", optC, [])

  fun a (optC, href, t) = let
    val attrs = [{key="href", value=href}]
    in
      ContainerTag ("a", optC, attrs, esc (str t))
    end

  fun title s = mk "title" (NONE, str s)

  fun link css = let
    val attrs = [{key="type", value="text/css"},
		 {key="rel", value="stylesheet"},
		 {key="href", value=css}]
    in
      SingletonTag ("link", NONE, attrs)
    end

  fun head (ttl, stylesheets) = let
    val contents = Sequence (title ttl :: (map link stylesheets))
    in
      ContainerTag ("head", NONE, [], contents)		    
    end

  val (body, bodyH, bodyCS, bodyCH) = mkMakers "body"

(* htdoc : string * string list * term -> term *)
  fun htdoc (ttl, stylesheets, contents) = let
    val h = head (ttl, stylesheets)
    val b = bodyH contents
    in
      ContainerTag ("html", NONE, [], Sequence [h, b])
    end

(* img : string -> term *)
  fun img src = SingletonTag ("img", NONE, [{key="src", value=src}])

(* comment : string -> term *)
  fun comment s = Comment s

  val toString = TL.toString

  val toFile = TL.toFile

  fun test 0 = let
        val sample = htdoc ("Test", 
			    ["core.css", "another.css"], 
			    Sequence [h1 "hello",
				      h2 "what's up?",
				      h3 "7 > 6",
				      pCS ("rockin", "Rock and roll.")])
        in
          print (toString sample);
	  print "\n";
	  toFile (sample, "look-at-me.html")
        end
    | test 1 = let
        val sample = Sequence [includeFile "prefix.html",
			       h1 "Hi.",
			       includeFile "suffix.html"]
        in
	  print (toString sample);
	  print "\n";
	  toFile (sample, "look-at-me.html")
        end
    | test n = print (concat ["No such test (",
			      Int.toString n,
			      ").\n"])

end
