(* tag-language.sml
 *
 * COPYRIGHT (c) 2008 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure TagLanguage = struct

  structure D = Doc

  type tag_name = string
  type class_name = string
  type attr = {key: string, value: string}

  datatype term
    = SingletonTag of tag_name * class_name option * attr list
    | ContainerTag of tag_name * class_name option * attr list * term
    | Sequence of term list
    | String of string
    | Comment of string

  fun atDoc {key, value} = (D.concat o map D.text) [key, "=\"", value, "\""]

  fun openTagDoc (tagName, optC, attrs) = let
    val c = (case optC 
	       of NONE => []
		| SOME c => [D.text (" class=\"" ^ c ^ "\"")])
    val ats = (case attrs
                 of [] => []
		  | _ => [D.space, D.concatWith D.space (map atDoc attrs)])
    in
      D.concat (D.text ("<" ^ tagName) :: c @ ats @ [D.text ">"])
    end

  fun closeTagDoc tagName = D.concat [D.text ("</" ^ tagName ^ ">")]

  fun comment c = D.concat (map D.text ["<!-- ", c, " -->"])

  fun toDoc tm = let
    infix ^^ 
    val op^^ = D.^^
    fun d (SingletonTag (t, optC, attrs)) = openTagDoc (t, optC, attrs)
      | d (ContainerTag (t, optC, attrs, tm as String s)) =
	  (openTagDoc (t, optC, attrs)) ^^ (d tm) ^^ (closeTagDoc t)	  
      | d (ContainerTag (t, optC, attrs, tm)) =
	  (openTagDoc (t, optC, attrs)) ^^ 
	  (D.nest 1 D.line) ^^
	  (D.nest 1 (d tm)) ^^ D.line ^^
	  (closeTagDoc t)
      | d (Sequence tms) = D.concatWith D.line (map toDoc tms)
      | d (String s) = D.text s
      | d (Comment c) = comment c
    in
      d tm
    end

  val toString = D.layout o toDoc

  fun toFile (t, outfile) = D.layoutToFile (toDoc t, outfile)

end
