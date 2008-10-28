(* mini-html.sig
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
 * I used an elisp program to assist in generating the repetitious specs; 
 * see the end of this file.
 *)

signature MINI_HTML = sig

  type class_name = string
  type attr = {key : string, value : string}
  type html 

  val nbsp : html

  val br : html

  val hr : class_name option -> html

  val str : string -> html

  val includeFile : string -> html

  val a : class_name option * string * string -> html

  val title : string -> html

(* link is for links to stylesheets *)
(* for hyperlinks, use "a" *)
  val link : string -> html

  val head : string * string list -> html

  val htdoc : string * string list * html -> html

  val img : string -> html

  val comment : string -> html

  val sequence : html list -> html

  val toString : html -> string

  val toFile : html * string -> unit

(* The convention followed below is
 *   h1   : string -> term
 *   h1H  : term -> term                (H for html)
 *   h1CS : class_name * string -> term (C for class, S for string)
 *   h1CH : class_name * term -> term   (C for class, H for html)
 *)

  val h1   : string -> html
  val h1H  : html -> html
  val h1CS : class_name * string -> html			   
  val h1CH : class_name * html -> html

  val h2   : string -> html
  val h2H  : html -> html
  val h2CS : class_name * string -> html
  val h2CH : class_name * html -> html

  val h3   : string -> html
  val h3H  : html -> html
  val h3CS : class_name * string -> html
  val h3CH : class_name * html -> html

  val h4   : string -> html
  val h4H  : html -> html
  val h4CS : class_name * string -> html
  val h4CH : class_name * html -> html

  val h5   : string -> html
  val h5H  : html -> html
  val h5CS : class_name * string -> html
  val h5CH : class_name * html -> html

  val h6   : string -> html
  val h6H  : html -> html
  val h6CS : class_name * string -> html
  val h6CH : class_name * html -> html

  val mkDiv : string -> html (* the id "div" is unavailable *)
  val divH  : html -> html
  val divCS : class_name * string -> html
  val divCH : class_name * html -> html
  val divCAH : class_name * attr list * html -> html

  val span   : string -> html
  val spanH  : html -> html
  val spanCS : class_name * string -> html
  val spanCH : class_name * html -> html

  val p   : string -> html
  val pH  : html -> html
  val pCS : class_name * string -> html
  val pCH : class_name * html -> html

  val blockquote   : string -> html
  val blockquoteH  : html -> html
  val blockquoteCS : class_name * string -> html
  val blockquoteCH : class_name * html -> html

  val table   : string -> html
  val tableH  : html -> html
  val tableCS : class_name * string -> html
  val tableCH : class_name * html -> html

  val tr   : string -> html
  val trH  : html -> html
  val trCS : class_name * string -> html
  val trCH : class_name * html -> html

  val th   : string -> html
  val thH  : html -> html
  val thCS : class_name * string -> html
  val thCH : class_name * html -> html

  val td   : string -> html
  val tdH  : html -> html
  val tdCS : class_name * string -> html
  val tdCH : class_name * html -> html

  val caption   : string -> html
  val captionH  : html -> html
  val captionCS : class_name * string -> html
  val captionCH : class_name * html -> html

  val ul   : string -> html
  val ulH  : html -> html
  val ulCS : class_name * string -> html
  val ulCH : class_name * html -> html

  val ol   : string -> html
  val olH  : html -> html
  val olCS : class_name * string -> html
  val olCH : class_name * html -> html

  val li   : string -> html
  val liH  : html -> html
  val liCS : class_name * string -> html
  val liCH : class_name * html -> html

  val body   : string -> html
  val bodyH  : html -> html
  val bodyCS : class_name * string -> html
  val bodyCH : class_name * html -> html

  val code   : string -> html
  val codeH  : html -> html
  val codeCS : class_name * string -> html
  val codeCH : class_name * html -> html

  val test : int -> unit

end

(*
;; helpful elisp code

(defun hfuncs (name)
  "Generate a bunch of ML specs for given NAME."
  (interactive "sName (e.g. h1): ")
  (princ (concat "val " name "   : string -> html\n"
		 "val " name "H  : html -> html\n"
		 "val " name "CS : class_name * string -> html\n"
		 "val " name "CH : class_name * html -> html\n\n")
	 (current-buffer)))

(mapc 'hfuncs '("ul" "ol" "li"))
;; mapc evaluates for side-effects only, like ML's List.app,
;; except it returns the input list

*)
