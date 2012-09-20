(* float-lit.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Internal representation of floating-point literals with limited
 * support for arithmetic.
 *)

structure FloatLit :> sig

    type float

    val isZero : float -> bool

  (* return the representation of +/-0.0 *)
    val zero : bool ->float

  (* plus and minus one *)
    val one : float
    val m_one : float

  (* negate a float *)
    val negate : float -> float

  (* equality, comparisons, and hashing functions *)
    val same : (float * float) -> bool
    val compare : (float * float) -> order
    val hash : float -> word

  (* create a float from pieces: isNeg is true if the number is negative, whole
   * is the whole-number part, frac is the fractional part, and exp is the
   * exponent.  This function may raise Overflow, when the exponent of the
   * normalized representation is too small or too large.
   *)
    val float : {isNeg : bool, whole : string, frac : string, exp : int} -> float
    val toString : float -> string

  (* external representation (for pickling) *)
    val toBytes : float -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> float

  end = struct

    structure SS = Substring
    structure W = Word
    structure W8V = Word8Vector

  (* The value {isNeg, digits=[d0, ..., dn], exp} represents the number
   *
   *	[+/-] 0.d0...dn * 10^exp
   *
   * where the sign is negative if isNeg is true.
   *)
    type float = {isNeg : bool, digits : int list, exp : int}

    fun isZero {isNeg, digits=[0], exp} = true
      | isZero _ = false

    fun zero isNeg = {isNeg = isNeg, digits = [0], exp = 0}

    val one = {isNeg = false, digits = [1], exp = 1}
    val m_one = {isNeg = true, digits = [1], exp = 1}

  (* negate a float *)
    fun negate {isNeg, digits, exp} =
	  {isNeg = not isNeg, digits = digits, exp = exp}

  (* equality, comparisons, and hashing functions *)
    fun same (f1 : float, f2 : float) =
	  (#isNeg f1 = #isNeg f2) andalso (#exp f1 = #exp f2)
	  andalso (#digits f1 = #digits f2)

    fun compare (f1 : float, f2 : float) = (case (#isNeg f1, #isNeg f2)
	   of (false, true) => GREATER
	    | (true, false) => LESS
	    | _ => (case Int.compare(#exp f1, #exp f2)
		 of EQUAL => let
		      fun cmp ([], []) = EQUAL
			| cmp ([], _) = LESS
			| cmp (_, []) = GREATER
			| cmp (d1::r1, d2::r2) = (case Int.compare(d1, d2)
			     of EQUAL => cmp(r1, r2)
			      | order => order
			    (* end case *))
		      in
			cmp (#digits f1, #digits f2)
		      end
		  | order => order
		(* end case *))
	  (* end case *))

    fun hash {isNeg, digits, exp} = let
	  fun hashDigits ([], h, _) = h
	    | hashDigits (d::r, h, i) =
		hashDigits (r, W.<<(W.fromInt d, i+0w4), W.andb(i+0w1, 0wxf))
	  in
	    hashDigits(digits, W.fromInt exp, 0w0)
	  end

    fun float {isNeg, whole, frac, exp} = let
	  fun cvtDigit (c, l) = (Char.ord c - Char.ord #"0") :: l
	  fun isZero #"0" = true | isZero _ = false
	(* whole digits with leading zeros removed *)
	  val whole = SS.dropl isZero (SS.full whole)
	(* fractional digits with trailing zeros removed *)
	  val frac = SS.dropr isZero (SS.full frac)
	(* normalize by stripping leading zero digits *)
	  fun normalize {isNeg, digits=[], exp} = zero isNeg
	    | normalize {isNeg, digits=0::r, exp} =
		normalize {isNeg=isNeg, digits=r, exp=exp-1}
	    | normalize flt = flt
	  in
	    case SS.foldr cvtDigit (SS.foldr cvtDigit [] frac) whole
	     of [] => zero isNeg
	      | digits => normalize {
		    isNeg = isNeg,
		    digits = digits,
		    exp = exp + SS.size whole
		  }
	    (* end case *)
	  end

    fun toString {isNeg, digits, exp} = let
	  val s = if isNeg then "-0." else "0."
	  val e = if exp < 0
		then ["e-", Int.toString(~exp)]
		else ["e", Int.toString exp]
	  in
	    concat(s :: List.foldr (fn (d, ds) => Int.toString d :: ds) e digits)
	  end



  (***** external representation (for pickling) *****
   *
   * The representation we use is a sequence of bytes:
   *
   *    [sign, d0, ..., dn, exp0, ..., exp3]
   *
   * where
   *    sign	== 0 or 1
   *    di      == ith digit
   *    expi    == ith byte of exponent (exp0 is lsb, exp3 is msb).
   *
   * NOTE: we could pack the sign and digits into 4-bit nibbles, but we are keeping
   * things simple for now.
   *)

    fun toBytes {isNeg, digits, exp} = let
	  val sign = if isNeg then 0w1 else 0w0
	  val digits = List.map Word8.fromInt digits
	  val exp' = W.fromInt exp
	  fun byte i = Word8.fromLargeWord(W.toLargeWord((W.>>(exp', 0w8*i))))
	  val exp = [byte 0w0, byte 0w1, byte 0w2, byte 0w3]
	  in
	    Word8Vector.fromList(sign :: (digits @ exp))
	  end

    fun fromBytes v = let
	  val ndigits = W8V.length v - 5
	  fun error () = raise Fail "Bogus float pickle"
	  val _ = if (ndigits < 1) then error() else ()
	  val isNeg = (case W8V.sub(v, 0)
		 of 0w0 => false
		  | 0w1 => true
		  | _ => error()
		(* end case *))
	  fun digit i = Word8.toInt(W8V.sub(v, i+1))
	  fun byte i = W.<<(
		W.fromLargeWord(Word8.toLargeWord(W8V.sub(v, ndigits+1+i))),
		W.fromInt(8*i))
	  val exp = W.toIntX(W.orb(byte 3, W.orb(byte 2, W.orb(byte 1, byte 0))))
	  in
	    {isNeg = isNeg, digits = List.tabulate(ndigits, digit), exp = exp}
	  end

  end

