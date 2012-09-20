(* integer-lit.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure IntegerLit :> sig

    type integer = IntInf.int

  (* range checks *)
    val isInt : integer -> bool		(* representable in 32-bits *)
    val isLong : integer -> bool	(* representable in 64-bits *)

  (* equality, comparisons, and hashing functions *)
    val same : (integer * integer) -> bool
    val compare : (integer * integer) -> order
    val hash : integer -> word

    val toString : integer -> string

  end = struct

    type integer = IntInf.int

  (* range checks *)
    fun isInt (n : IntInf.int) =
	  (~0x80000000 <= n) andalso (n < 0x100000000)
    fun isLong (n : IntInf.int) =
	  (~0x8000000000000000 <= n) andalso (n < 0x10000000000000000)

  (* equality, comparisons, and hashing functions *)
    fun same (a : IntInf.int, b) = (a = b)
    val compare = IntInf.compare
    fun hash i = Word.fromInt(IntInf.toInt(IntInf.andb(i, 0xfffffff)))

    fun toString i = if (i < 0)
	  then "-" ^ IntInf.toString(IntInf.~ i)
	  else IntInf.toString i

  end
