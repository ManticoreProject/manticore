(* bool.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Bool : BOOL =
  struct

    datatype bool = datatype bool

    val not = _prim(BNot)

    fun scan (getc : (char, 'a) StringCvt.reader) cs = (
	  case (getc (PreStringCvt.skipWS getc cs))
	   of (SOME(#"t", cs')) => (case (PreStringCvt.getNChars getc (cs', 3))
		 of (SOME([#"r", #"u", #"e"], cs'')) => SOME(true, cs'')
		  | _ => NONE
		(* end case *))
	    | (SOME(#"f", cs')) => (case (PreStringCvt.getNChars getc (cs', 4))
		 of (SOME([#"a", #"l", #"s", #"e"], cs'')) => SOME(false, cs'')
		  | _ => NONE
		(* end case *))
	    | _ => NONE
	  (* end case *))

    val toString : bool -> string

    val fromString = PreStringCvt.scanString scan

    fun toString x = if x then "true" else "false"

  end
