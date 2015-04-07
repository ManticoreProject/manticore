(* pre-string-cvt.sml
 *
 * COPYRIGHT (c) 2015 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * This module has some code to support string conversions that is used by basis
 * modules that are compiled before StringCvt is available.
 *)

structure PreStringCvt : sig

    type ('a, 'b) reader = 'b -> ('a * 'b) option

    val skipWS : (char, 'strm) reader -> 'strm -> 'strm

    val getNChars : (char, 'strm) reader -> ('strm * int) -> char list

    val scanString : ((char, int) reader -> ('a, int) reader) -> string -> 'a option

  end = struct

    type ('a, 'b) reader = 'b -> ('a * 'b) option

    fun skipWS (getc : (char, 'strm) reader) = let
	  fun isWS (#" ") = true
	    | isWS (#"\t") = true
	    | isWS (#"\n") = true
	    | isWS _ = false
	  fun lp cs = (case (getc cs)
		 of (SOME(c, cs')) => if (isWS c) then lp cs' else cs
		  | NONE => cs
		(* end case *))
	  in
	    lp
	  end

  (* get n characters from a character source (this is not a visible part of
   * StringCvt.
   *)
    fun getNChars (getc : (char, 'strm) reader) (cs, n) = let
	  fun rev ([], l2) = l2
	    | rev (x::l1, l2) = rev(l1, x::l2)
	  fun get (cs, 0, l) = SOME(rev(l, []), cs)
	    | get (cs, i, l) = (case getc cs
		 of NONE => NONE
		  | (SOME(c, cs')) => get (cs', i-1, c::l)
		(* end case *))
	  in
	    get (cs, n, [])
	  end

    fun scanString scanFn s = let
	  val n = InlineT.CharVector.length s
	  fun getc i = if (i < n) then SOME(CharVector.sub(s, i), i+1) else NONE
	  in
	    case (scanFn getc 0)
	     of NONE => NONE
	      | SOME(x, _) => SOME x
	    (* end case *)
	  end

  end
