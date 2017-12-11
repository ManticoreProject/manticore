(* signed-wrapping-arith.sml
 *
 * Implements signed, trapping arithmetic.
 *
 * COPYRIGHT (c) 2017 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * This code is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *)

structure SignedWrappingArith : SIGNED_CONST_ARITH =
  struct

    type t = IntInf.int
    type width = int

    fun pow2 w = IntInf.<<(1, Word.fromInt w)

  (* narrow the representation of n to `wid` bits (2's complement).  This behaves
   * like a C-style cast to a signed integer type.
   *)
    fun sNarrow (wid, n) = let
	  val limit = pow2 wid
          val n = IntInf.andb(n, limit - 1)
	  in
	    if n < pow2(wid - 1) then n else n - limit
	  end

    fun toSigned (wid, a) = if a < pow2(wid - 1)
	  then a
	  else a - pow2 wid

    fun sAdd (wid, a, b) = sNarrow (wid, a + b)
    fun sSub (wid, a, b) = sNarrow (wid, a - b)
    fun sMul (wid, a, b) = sNarrow (wid, a * b)
    fun sDiv (wid, a, b) = sNarrow (wid, a div b)
    fun sMod (_, 0, 0) = raise Div (* workaround for bug in SML/NJ pre 110.82 *)
      | sMod (wid, a, b) = sNarrow (wid, a mod b)
    fun sQuot (wid, a, b) = sNarrow (wid, IntInf.quot(a, b))
    fun sRem (_, 0, 0) = raise Div (* workaround for bug in SML/NJ pre 110.82 *)
      | sRem (wid, a, b) = sNarrow (wid, IntInf.rem(a, b))
    fun sNeg (wid, a) = sNarrow (wid, ~a)
    fun sAbs (wid, a) = if (a < 0) then sNarrow (wid, ~a) else a

  (* signed left-shift operation. Shift amounts that are >= wid result in zero. *)
    fun sShL (wid, a, b) =
          if (b >= IntInf.fromInt wid)
            then 0
            else sNarrow (wid, IntInf.<<(a, Word.fromLargeInt b))

  (* signed right-shift operation. Shift amounts that are >= wid result in zero. *)
    fun sShR (wid, a, b) = let
          val shft = Word.fromLargeInt(IntInf.min(b, IntInf.fromInt wid))
 	  in
	    sNarrow (wid, IntInf.~>>(a, shft))
	  end

  end
