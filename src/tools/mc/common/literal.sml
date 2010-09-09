(* literal.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Literal values.
 *)

structure Literal : sig

    datatype literal
      = Enum of word			(* tagged ints used for constants *)
      | StateVal of word		(* special constant used to represent a state value. *)
					(* The runtime representation of this value will be *)
					(* distinct from both boxed and unboxed values. *)
      | Tag of string                   (* special constant used to distinguish elements *)
                                        (* in association lists. *)
      | Int of IntegerLit.integer	(* Int, Long, and Integer types *)
      | Float of FloatLit.float		(* Float and Double types *)
      | Char of UTF8.wchar
      | String of string		(* uses UTF8 encoding *)
      | Bool of bool			(* Raw boolean value (not PML's bool type!) *)

    val toString : literal -> string

    val same : (literal * literal) -> bool
    val compare : (literal * literal) -> order
    val hash : literal -> word

    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = literal

  (* some standard constants *)
    val unitLit : literal
    val trueLit : literal
    val falseLit : literal

  end = struct

    datatype literal
      = Enum of word			(* tagged ints used for constants *)
      | StateVal of word		(* special constant used to represent a state value. *)
					(* The runtime representation of this value will be *)
					(* distinct from both boxed and unboxed values. *)
      | Tag of string                   (* special constant used to distinguish elements *)
                                        (* in association lists. *)
      | Int of IntegerLit.integer	(* Int, Long, and Integer types *)
      | Float of FloatLit.float		(* Float and Double types *)
      | Char of UTF8.wchar
      | String of string		(* uses UTF8 encoding *)
      | Bool of bool			(* Raw boolean value (not PML's bool type!) *)

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
		    else concat["\\(0x", Word.toString w, ")"]
	      (* end case *))
	    else concat["\\(0x", Word.toString w, ")"]

    fun utf8ToStr s =
	  concat(rev(UTF8.fold (fn (w, l) => wcharToStr w :: l) [] s))

    fun toString (Enum n) = concat["enum(", Word.fmt StringCvt.DEC n, ")"]
      | toString (StateVal n) = "$" ^ Word.fmt StringCvt.DEC n
      | toString (Tag t) = concat ["tag(", t, ")"]
      | toString (Int i) = IntegerLit.toString i
      | toString (Float flt) = FloatLit.toString flt
      | toString (Char wc) = concat["'", wcharToStr wc, "'"]
      | toString (String s) = concat["\"", utf8ToStr s, "\""]
      | toString (Bool true) = "TRUE"
      | toString (Bool false) = "FALSE"

    fun same (Enum e1, Enum e2) = (e1 = e2)
      | same (StateVal n1, StateVal n2) = (n1 = n2)
      | same (Tag n1, Tag n2) = (n1 = n2)
      | same (Int i1, Int i2) = IntegerLit.same(i1, i2)
      | same (Float f1, Float f2) = FloatLit.same(f1, f2)
      | same (Char c1, Char c2) = (c1 = c2)
      | same (String s1, String s2) = (s1 = s2)
      | same (Bool b1, Bool b2) = (b1 = b2)
      | same _ = false

    fun compare (Enum a, Enum b) = Word.compare(a, b)
      | compare (StateVal n1, StateVal n2) = Word.compare(n1, n2)
      | compare (Tag n1, Tag n2) = String.compare (n1, n2)
      | compare (Int i1, Int i2) = IntegerLit.compare(i1, i2)
      | compare (Float f1, Float f2) = FloatLit.compare(f1, f2)
      | compare (Char c1, Char c2) = Word.compare(c1, c2)
      | compare (String s1, String s2) = String.compare(s1, s2)
      | compare (Bool false, Bool true) = LESS
      | compare (Bool true, Bool false) = GREATER
      | compare (Bool _, Bool _) = EQUAL
      | compare (Enum _, _) = LESS
      | compare (_, Enum _) = GREATER
      | compare (StateVal _, _) = LESS
      | compare (_, StateVal _) = GREATER
      | compare (Tag _, _) = LESS
      | compare (_, Tag _) = GREATER
      | compare (Int _, _) = LESS
      | compare (_, Int _) = GREATER
      | compare (Float _, _) = LESS
      | compare (_, Float _) = GREATER
      | compare (Char _, _) = LESS
      | compare (_, Char _) = GREATER
      | compare (String _, _) = LESS
      | compare (_, String _) = GREATER

  (* for hash codes, use the low-order 4 bits for a type code *)
    local
      val enumCd = 0w2
      val stateCd = 0w3
      val intCd = 0w5
      val floatCd = 0w7
      val charCd = 0w11
      val stringCd = 0w13
      val boolCd = 0w17
      val tagCd = 0w15
      fun h (hash, base) = Word.<<(hash, 0w4) + base
    in
    fun hash (Enum w) = h(w, enumCd)
      | hash (StateVal w) = h(w, stateCd)
      | hash (Tag w) = h(HashString.hashString w, tagCd)
      | hash (Int i) = h(IntegerLit.hash i, intCd)
      | hash (Float f) = h(FloatLit.hash f, floatCd)
      | hash (Char c) = h(c, charCd)
      | hash (String s) = h(HashString.hashString s, stringCd)
      | hash (Bool false) = h(0w1, boolCd)
      | hash (Bool true) = h(0w3, boolCd)
    end (* local *)

    structure Tbl = HashTableFn (
      struct
	type hash_key = literal
	val hashVal = hash
	val sameKey = same
      end)

  (* some standard constants *)
    val unitLit = Enum 0w0
    val trueLit = Bool true
    val falseLit = Bool false

  end
