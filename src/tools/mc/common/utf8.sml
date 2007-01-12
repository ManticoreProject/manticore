(* utf8.sml
 *
 * COPYRIGHT (c) 2004 The Moby Project (moby.cs.uchicago.edu)
 * All rights reserved.
 *
 * Routines for working with UTF8 encoded strings.
 *
 *	Unicode value		1st byte    2nd byte    3rd byte    4th byte
 *	-----------------	--------    --------    --------    --------
 *	00000000 0xxxxxxx	0xxxxxxx	
 *	00000yyy yyxxxxxx	110yyyyy    10xxxxxx
 *	zzzzyyyy yyxxxxxx	1110zzzz    10yyyyyy	10xxxxxx
 *	110110ww wwzzzzyy+
 *	110111yy yyxxxxxx	11110uuu    10uuzzzz	10yyyyyy    10xxxxxx!
 *
 * (!) where uuuuu = wwww+1
 *
 * TODO:
 *    Add support for surrogate pairs (this will require changing the type
 *    of wchar to Word32.word).
 *)

structure UTF8 :> UTF8 =
  struct

    structure W = Word
    structure SS = Substring

    type wchar = W.word

    exception Incomplete
	(* raised by some operations when applied to incomplete strings. *)

    fun getContByte (ss, wc) = (case (SS.getc ss)
	   of NONE => raise Incomplete
	    | SOME(c, ss') => let
		val b = W.fromInt(Char.ord c)
		in
		  if (W.andb(0wxc0, b) = 0wx80)
		    then (ss', W.orb(W.<<(wc, 0w6), W.andb(0wx3f, b)))
		    else raise Incomplete
		end
	  (* end case *))

  (* fold a function over the Unicode characters in the string *)
    fun fold f = let
	  fun foldf (ss, acc) = (case SS.getc ss
		 of NONE => acc
		  | SOME(c, ss) => let
		      val w = W.fromInt(Char.ord c)
		      in
			if (w < 0w128)
			  then foldf (ss, f(w, acc))
			else (case (W.andb(0wxe0, w))
			   of 0wxc0 => let
				val (ss, wc) = getContByte(ss, W.andb(0wx1f, w))
				in
				  foldf (ss, f(wc, acc))
				end
			    | 0wxe0 => let
				val (ss, wc) =
				      getContByte(
					getContByte(ss, W.andb(0wx0f, w)))
				in
				  foldf (ss, f(wc, acc))
				end
			    | _ => raise Incomplete
			  (* end case *))
		      end
		(* end case *))
	  in
	    fn init => fn s => foldf (SS.full s, init)
	  end

  (* return the list of wide characters that are encoded by a string *)
    fun explode s = rev(fold (op ::) [] s)

  (* return the number of Unicode characters *)
    fun size s = fold (fn (_, n) => n+1) 0 s

    fun w2c w = Char.chr(W.toInt w)

  (* return the UTF8 encoding of a wide character *)
    fun encode wc = if (W.<(wc, 0wx80))
	    then String.str(w2c wc)
	  else if (W.<(wc, 0wx800))
	    then String.implode[
		w2c(W.orb(0wxc0, W.>>(wc, 0w6))),
		w2c(W.orb(0wx80, W.andb(wc, 0wx3f)))
	      ]
	    else String.implode[
		w2c(W.orb(0wxe0, W.>>(wc, 0w12))),
		w2c(W.orb(0wx80, W.andb(W.>>(wc, 0w6), 0wx3f))),
		w2c(W.orb(0wx80, W.andb(wc, 0wx3f)))
	      ]

    fun isAscii (wc : wchar) = (wc < 0wx80)
    fun toAscii (wc : wchar) = w2c(W.andb(0wx7f, wc))
    fun fromAscii c = W.andb(0wx7f, W.fromInt(Char.ord c))

  (* return a printable string representation of a wide character *)
    fun toString wc =
	  if isAscii wc
	    then Char.toCString(toAscii wc)
	    else "\\u" ^ (StringCvt.padLeft #"0" 4 (W.toString wc))

  end

