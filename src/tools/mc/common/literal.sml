(* literal.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Literal values.
 *)

structure Literal : sig

    datatype literal
      = Bool of bool
      | Int of IntegerLit.integer	(* Int, Long, and Integer types *)
      | Float of FloatLit.float		(* Float and Double types *)
      | Char of UTF8.wchar
      | String of string		(* uses UTF8 encoding *)

    val toString : literal -> string

    val same : (literal * literal) -> bool
    val compare : (literal * literal) -> order
    val hash : literal -> word

    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = literal

  end = struct

    datatype literal
      = Bool of bool
      | Int of IntegerLit.integer	(* Int, Long, and Integer types *)
      | Float of FloatLit.float		(* Float and Double types *)
      | Char of UTF8.wchar
      | String of string		(* uses UTF8 encoding *)

  (* convert a wide character to its ASCII representation as a Manticore string *)
    fun wcharToStr (w : UTF8.wchar) =
	  if (UTF8.isAscii w)
	    then (case (UTF8.toAscii w)
	       of #"\"" => "\\\""
		| #"\\" => "\\\\"
		| #"\008" => "\\b"
		| #"\009" => "\\t"
		| #"\010" => "\\n"
		| #"\012" => "\\f"
		| #"\013" => "\\r"
		| c => if (Char.isPrint c)
		    then String.str c
		    else concat["\\(0x", Word32.toString w, ")"]
	      (* end case *))
	    else concat["\\(0x", Word32.toString w, ")"]

    fun utf8ToStr s =
	  concat(rev(UTF8.fold (fn (w, l) => wcharToStr w :: l) [] s))

    fun toString (Bool true) = "True"
      | toString (Bool false) = "False"
      | toString (Int i) = IntegerLit.toString i
      | toString (Float flt) = FloatLit.toString flt
      | toString (Char wc) = concat["'", wcharToStr wc, "'"]
      | toString (String s) = concat["\"", utf8ToStr s, "\""]

    fun same (Bool b1, Bool b2) = (b1 = b2)
      | same (Int i1, Int i2) = IntegerLit.same(i1, i2)
      | same (Float f1, Float f2) = FloatLit.same(f1, f2)
      | same (Char c1, Char c2) = (c1 = c2)
      | same (String s1, String s2) = (s1 = s2)
      | same _ = false

    fun compare (Bool false, Bool true) = LESS
      | compare (Bool true, Bool false) = GREATER
      | compare (Bool _, Bool _) = EQUAL
      | compare (Int i1, Int i2) = IntegerLit.compare(i1, i2)
      | compare (Float f1, Float f2) = FloatLit.compare(f1, f2)
      | compare (Char c1, Char c2) = Word32.compare(c1, c2)
      | compare (String s1, String s2) = String.compare(s1, s2)
      | compare (Bool _, _) = LESS
      | compare (_, Bool _) = GREATER
      | compare (Int _, _) = LESS
      | compare (_, Int _) = GREATER
      | compare (Float _, _) = LESS
      | compare (_, Float _) = GREATER
      | compare (Char _, _) = LESS
      | compare (_, Char _) = GREATER

  (* for hash codes, use the low-order 3 bits for a type code *)
    local
      val unitCd = 0w1
      val boolCd = 0w2
      val intCd = 0w3
      val floatCd = 0w4
      val charCd = 0w5
      val stringCd = 0w6
      fun h (hash, base) = Word.<<(hash, 0w3) + base
    in
    fun hash (Bool false) = h(0w0, boolCd)
      | hash (Bool true) = h(0w1, boolCd)
      | hash (Int i) = h(IntegerLit.hash i, intCd)
      | hash (Float f) = h(FloatLit.hash f, floatCd)
      | hash (Char c) = h(Word.fromLargeWord(Word32.toLargeWord c), charCd)
      | hash (String s) = h(HashString.hashString s, stringCd)
    end (* local *)

    structure Tbl = HashTableFn (
      struct
	type hash_key = literal
	val hashVal = hash
	val sameKey = same
      end)

  end
