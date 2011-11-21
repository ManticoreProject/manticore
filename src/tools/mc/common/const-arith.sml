(* const-arith.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Arithmetic on integer constants.
 *)

structure ConstArith : sig

    type integer = IntegerLit.integer

    val i32Min : integer	(* -2^31 *)
    val i32Max : integer	(* 2^31-1 *)
    val i64Min : integer	(* -2^63 *)
    val i64Max : integer	(* 2^63-1 *)

    val powerOfTwo : integer -> integer option

    val iNeg : integer -> integer
    val iAdd : integer * integer -> integer
    val iSub : integer * integer -> integer
    val iMul : integer * integer -> integer
    val iDiv : integer * integer -> integer
    val iMod : integer * integer -> integer
    val iNot : integer -> integer
    val iAnd : integer * integer -> integer
    val iOr  : integer * integer -> integer
    val iXOr : integer * integer -> integer
    val iEq  : integer * integer -> bool
    val iNeq : integer * integer -> bool
    val iLt  : integer * integer -> bool
    val iLte : integer * integer -> bool
    val iGt  : integer * integer -> bool
    val iGte : integer * integer -> bool

  (* convert signed 32-bit literal to unsigned *)
    val i32ToUnsigned : integer -> integer

    val i32Neg : integer -> integer
    val i32Add : integer * integer -> integer
    val i32Sub : integer * integer -> integer
    val i32Mul : integer * integer -> integer
    val i32Div : integer * integer -> integer
    val i32Mod : integer * integer -> integer
    val i32Not : integer -> integer
    val i32And : integer * integer -> integer
    val i32Or  : integer * integer -> integer
    val i32XOr : integer * integer -> integer

    val i64Neg : integer -> integer
    val i64Add : integer * integer -> integer
    val i64Sub : integer * integer -> integer
    val i64Mul : integer * integer -> integer
    val i64Div : integer * integer -> integer
    val i64Mod : integer * integer -> integer
    val i64Not : integer -> integer
    val i64And : integer * integer -> integer
    val i64Or  : integer * integer -> integer
    val i64XOr : integer * integer -> integer

  end = struct

    structure I = IntInf

    type integer = IntInf.int

    val int_2_32 : integer	= I.pow(2, 32)
    val int_2_33 : integer	= I.pow(2, 33)
    val int_2_64 : integer	= I.pow(2, 64)

    val i32Min : integer	= ~(I.pow(2, 31))
    val i32Max : integer	= I.pow(2, 31) - 1
    val i64Min : integer	= ~(I.pow(2, 63))
    val i64Max : integer	= I.pow(2, 63) - 1

    fun powerOfTwo n = let
	  val l2 = I.log2 n
	  in
	    if (I.pow(2, l2) = n)
	      then SOME(I.fromInt l2)
	      else NONE
	  end

    val iNeg : integer -> integer		= I.~
    val iAdd : integer * integer -> integer	= I.+
    val iSub : integer * integer -> integer	= I.-
    val iMul : integer * integer -> integer	= I.*
    val iDiv : integer * integer -> integer	= I.quot
    val iMod : integer * integer -> integer	= I.rem
    val iNot : integer -> integer		= I.notb
    val iAnd : integer * integer -> integer	= I.andb
    val iOr  : integer * integer -> integer	= I.orb
    val iXOr : integer * integer -> integer	= I.xorb
    val iEq  : integer * integer -> bool	= (op =)
    val iNeq : integer * integer -> bool	= (op <>)
    val iLt  : integer * integer -> bool	= I.<
    val iLte : integer * integer -> bool	= I.<=
    val iGt  : integer * integer -> bool	= I.>
    val iGte : integer * integer -> bool	= I.>=

    local
    (* convert to unsigned representation *)
      fun to32 a = if (a < 0) then iAdd(int_2_32, a) else a
    (* convert from unsigned representation *)
      fun from32 a = if (i32Max < a) then iSub(a, int_2_32) else a
    in
    val i32ToUnsigned = to32
    fun i32Neg a = if (a = i32Min) then a else iNeg a
    fun i32Add (a, b) = from32(iMod(iAdd(to32 a, to32 b), int_2_32))
    fun i32Sub (a, b) = from32(iMod(iAdd(to32 a, to32(iNeg b)), int_2_32))
    fun i32Mul (a, b) = from32(iMod(iMul(to32 a, to32 b), int_2_32))
    fun i32Div (a, b) = (case (I.sign a, I.sign b)
	   of (~1, ~1) => from32(iDiv(iNeg a, iNeg b))
	    | (~1, _) => from32(iNeg(iDiv(iNeg a, b)))
	    | (_, ~1) => from32(iNeg(iDiv(a, iNeg b)))
	    | _ => from32(iDiv(a, b))
	  (* end case *))
    fun i32Mod (a, b) = (case (I.sign a, I.sign b)
	   of (~1, ~1) => iNeg(iMod(iNeg a, iNeg b))
	    | (~1, _) => iNeg(iMod(iNeg a, b))
	    | (_, ~1) => iMod(a, iNeg b)
	    | _ => iMod(a, b)
	  (* end case *))
    fun i32Not a = iNot a
    fun i32And (a, b) = from32(iAnd(to32 a, to32 b))
    fun i32Or (a, b) = from32(iOr(to32 a, to32 b))
    fun i32XOr (a, b) = from32(iXOr(to32 a, to32 b))
    end

    fun i64Neg (a) = raise Fail "i64Neg"
    fun i64Add (a, b) = raise Fail "i64Add"
    fun i64Sub (a, b) = raise Fail "i64Sub"
    fun i64Mul (a, b) = raise Fail "i64Mul"
    fun i64Div (a, b) = raise Fail "i64Div"
    fun i64Mod (a, b) = raise Fail "i64Mod"
    fun i64Not (a) = raise Fail "i64Not"
    fun i64And (a, b) = raise Fail "i64And"
    fun i64Or (a, b) = raise Fail "i64Or"
    fun i64XOr (a, b) = raise Fail "i64XOr"

  end
